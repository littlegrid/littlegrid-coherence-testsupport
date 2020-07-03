/*
 * Copyright (c) 2010-2020 Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the littlegrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid.impl;

import com.tangosol.net.CacheFactory;
import org.littlegrid.CallbackHandler;
import org.littlegrid.ClusterMember;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupBuildException;
import org.littlegrid.IdentifiableException;
import org.littlegrid.support.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static java.lang.String.format;
import static org.littlegrid.IdentifiableException.ReasonEnum.CHECK_CHILD_FIRST_CLASS_PATH_IN_USE;

/**
 * Default local process cluster member group implementation.
 */
public class DefaultClusterMemberGroup implements ClusterMemberGroup {
    private static final int ONE_MB = 1024 * 1024;
    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultClusterMemberGroup.class);

    private final List<Future<DelegatingClusterMemberWrapper>> memberFutures = new ArrayList<>();

    private final CallbackHandler callbackHandler;
    private final Properties systemPropertiesBeforeStartAllInvoked;
    private final int sleepAfterStopDuration35x;
    private final int sleepAfterStopDuration36x;
    private final int sleepAfterStopDurationDefault;
    private final int wkaPort;
    private final int extendPort;

    private boolean startAllInvoked;
    private boolean shutdownAllInvoked;

    /**
     * Constructor.
     *
     * @param callbackHandler               Callback handler.
     * @param sleepAfterStopDuration35x     Sleep duration for 3.5.x.
     * @param sleepAfterStopDuration36x     Sleep duration for 3.6.x.
     * @param sleepAfterStopDurationDefault Default sleep duration.
     * @param wkaPort                       WKA port.
     * @param extendPort                    Extend port.
     */
    public DefaultClusterMemberGroup(final CallbackHandler callbackHandler,
                                     final int sleepAfterStopDuration35x,
                                     final int sleepAfterStopDuration36x,
                                     final int sleepAfterStopDurationDefault,
                                     final int wkaPort,
                                     final int extendPort) {

        this.wkaPort = wkaPort;
        this.extendPort = extendPort;

        if (callbackHandler == null) {
            throw new IllegalArgumentException("Callback handler cannot be null");
        }

        this.callbackHandler = callbackHandler;
        this.sleepAfterStopDuration35x = sleepAfterStopDuration35x;
        this.sleepAfterStopDuration36x = sleepAfterStopDuration36x;
        this.sleepAfterStopDurationDefault = sleepAfterStopDurationDefault;

        systemPropertiesBeforeStartAllInvoked = SystemUtils.snapshotSystemProperties();

        callbackHandler.doBeforeStart();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int merge(final ClusterMemberGroup otherMemberGroup) {
        if (shutdownAllInvoked) {
            throw new UnsupportedOperationException("Shutdown all has been called - cannot now accept groups to merge");
        }

        final DefaultClusterMemberGroup defaultClusterMemberGroup = (DefaultClusterMemberGroup) otherMemberGroup;

        LOGGER.info("About to merge - current members started: {}, members started to merge in: {}",
                this.getStartedMemberIds().length, defaultClusterMemberGroup.getMemberFutures().size());

        merge(defaultClusterMemberGroup.getMemberFutures());

        return memberFutures.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClassLoader[] getActualContainingClassLoaders(final int... memberIds) {
        final List<ClassLoader> classLoaders = new ArrayList<>();

        for (final int memberId : memberIds) {
            final ClusterMember member = getClusterMember(memberId);

            if (member != null) {
                classLoaders.add(member.getActualContainingClassLoader());
            }
        }

        return classLoaders.toArray(new ClassLoader[classLoaders.size()]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return wkaPort;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendPort() {
        return extendPort;
    }

    int merge(final List<Future<DelegatingClusterMemberWrapper>> memberFuturesToAdd) {
        memberFutures.addAll(memberFuturesToAdd);

        return memberFutures.size();
    }

    /**
     * Reduced scope method to get this members list of futures, the references to the
     * cluster members.
     *
     * @return list of member's futures.
     */
    List<Future<DelegatingClusterMemberWrapper>> getMemberFutures() {
        return memberFutures;
    }

    /**
     * Starts all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup startAll() {
        if (startAllInvoked) {
            return this;
        }

        startAllInvoked = true;
        shutdownAllInvoked = false;

        callbackHandler.doAfterStart();

        return this;
    }

    static List<Future<DelegatingClusterMemberWrapper>> startClusterMembers(
            final int numberOfMembers,
            final Properties systemPropertiesToBeApplied,
            final URL[] classPathUrls,
            final String clusterMemberInstanceClassName,
            final int numberOfThreadsInStartUpPool) {

        if (numberOfMembers < 1) {
            throw new IllegalArgumentException("Number of members must be 1 or more");
        }

        if (systemPropertiesToBeApplied == null || systemPropertiesToBeApplied.size() == 0) {
            throw new IllegalArgumentException("No system properties specified, cannot setup cluster");
        }

        if (classPathUrls == null || classPathUrls.length == 0) {
            throw new IllegalArgumentException("No class path URLs specified - will not be able to load classes");
        }

        if (clusterMemberInstanceClassName == null || clusterMemberInstanceClassName.trim().length() == 0) {
            throw new IllegalArgumentException("No cluster member instance class name, cannot setup cluster");
        }

        if (numberOfThreadsInStartUpPool < 1) {
            throw new IllegalArgumentException("Invalid number of threads specified for start-up pool, cannot start");
        }

        final Properties systemPropertiesBeforeStartInvoked = SystemUtils.snapshotSystemProperties();
        final List<Future<DelegatingClusterMemberWrapper>> memberFutures = new ArrayList<>();

        SystemUtils.applyToSystemProperties(systemPropertiesToBeApplied);
        outputStartAllMessages(numberOfMembers, systemPropertiesToBeApplied, classPathUrls,
                numberOfThreadsInStartUpPool);

        try {
            final List<Callable<DelegatingClusterMemberWrapper>> tasks = new ArrayList<>(numberOfMembers);

            for (int i = 0; i < numberOfMembers; i++) {
                tasks.add(new ClusterMemberCallable(clusterMemberInstanceClassName, classPathUrls));
            }

            final Callable<DelegatingClusterMemberWrapper> taskForSeniorMember = tasks.remove(0);

            final ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreadsInStartUpPool);

            LOGGER.debug("About to establish a cluster using a single member initially");
            final Future<DelegatingClusterMemberWrapper> futureForSeniorMember =
                    executorService.submit(taskForSeniorMember);

            futureForSeniorMember.get();

            LOGGER.debug("First cluster member up, starting any remaining members to join established cluster");
            final List<Future<DelegatingClusterMemberWrapper>> futuresForOtherMembers =
                    executorService.invokeAll(tasks);

            memberFutures.add(futureForSeniorMember);
            memberFutures.addAll(futuresForOtherMembers);

            executorService.shutdown();

            ensureMemberIdsAreUnique(getStartedMemberIds(memberFutures));

            LOGGER.debug(format("This group of cluster member(s) started, member Ids: %s",
                    Arrays.toString(getStartedMemberIds(memberFutures))));
        } catch (Exception e) {
            LOGGER.error("Failed to start cluster member group - please check the exception report to aid diagnosis");

            throw new ClusterMemberGroupBuildException(e, systemPropertiesBeforeStartInvoked,
                    systemPropertiesToBeApplied, numberOfMembers, classPathUrls,
                    clusterMemberInstanceClassName, numberOfThreadsInStartUpPool);
        } finally {
            System.setProperties(systemPropertiesBeforeStartInvoked);
        }

        return memberFutures;
    }

    static void ensureMemberIdsAreUnique(final int[] memberIds) {
        Set<Integer> memberIdSet = new HashSet<>(memberIds.length);

        for (int memberId : memberIds) {
            memberIdSet.add(memberId);
        }

        if (memberIdSet.size() != memberIds.length) {
            throw new IdentifiableException(
                    format("There were %s member ids %s - however only these were unique member "
                                    + "ids %s.  Ensure that the Coherence JAR is on your test class path",
                            memberIds.length, Arrays.toString(memberIds), memberIdSet),
                    CHECK_CHILD_FIRST_CLASS_PATH_IN_USE);
        }
    }

    @SuppressWarnings("unchecked")
    private static void outputStartAllMessages(final int numberOfMembers,
                                               final Properties systemPropertiesToBeApplied,
                                               final URL[] classPathUrls,
                                               final int numberOfThreadsInStartUpPool) {
        LOGGER.debug("About to start {} cluster member(s) in group, using {} threads in pool",
                numberOfMembers, numberOfThreadsInStartUpPool);

        LOGGER.debug("Class path (after exclusions)..: {}", Arrays.deepToString(classPathUrls));
        LOGGER.info("System properties to be set.: {}", new TreeMap<>(systemPropertiesToBeApplied));
        outputMemoryMessage();
    }

    private static void outputMemoryMessage() {
        LOGGER.info("Max memory: {}MB, current: {}MB, free memory: {}MB",
                Runtime.getRuntime().maxMemory() / ONE_MB,
                Runtime.getRuntime().totalMemory() / ONE_MB,
                Runtime.getRuntime().freeMemory() / ONE_MB);
    }

    DelegatingClusterMemberWrapper getClusterMemberWrapper(final int memberId) {
        if (!startAllInvoked) {
            throw new IllegalStateException("Cluster member group never started");
        }

        try {
            for (final Future<DelegatingClusterMemberWrapper> task : memberFutures) {
                final DelegatingClusterMemberWrapper memberWrapper = task.get();

                if (memberWrapper.isRunning() && memberWrapper.getLocalMemberId() == memberId) {
                    return memberWrapper;
                }
            }
        } catch (Exception e) {
            throw new IllegalStateException("Unable to get specified member due to " + e);
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getStartedMemberIds() {
        return getStartedMemberIds(memberFutures);
    }

    static int[] getStartedMemberIds(final List<Future<DelegatingClusterMemberWrapper>> memberFutures) {
        try {
            final List<Integer> memberIds = new ArrayList<>();

            for (final Future<DelegatingClusterMemberWrapper> task : memberFutures) {
                final DelegatingClusterMemberWrapper memberWrapper = task.get();

                if (memberWrapper.isRunning()) {
                    memberIds.add(memberWrapper.getLocalMemberId());
                }
            }

            int[] memberIdsArray = new int[memberIds.size()];

            for (int i = 0; i < memberIds.size(); i++) {
                memberIdsArray[i] = memberIds.get(i);
            }

            return memberIdsArray;
        } catch (InterruptedException | ExecutionException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getSuggestedSleepAfterStopDuration() {
        return getSuggestedSleepDurationBasedUponVersion(getMajorMinorVersion());
    }

    /**
     * Gets the suggested sleep duration based upon the version.
     *
     * @param majorMinorVersion Version of Coherence.
     * @return returns the suggested sleep duration.
     */
    @Deprecated
    public int getSuggestedSleepDurationBasedUponVersion(final float majorMinorVersion) {
        final float coherenceVersionNumber36x = 3.6f;
        final float coherenceVersionNumber370 = 3.7f;

        if (majorMinorVersion < coherenceVersionNumber36x) {
            return sleepAfterStopDuration35x;

        } else if (majorMinorVersion < coherenceVersionNumber370) {
            return sleepAfterStopDuration36x;
        }

        return sleepAfterStopDurationDefault;
    }

    private static float getMajorMinorVersion() {
        final String majorMinorVersionString = CacheFactory.VERSION.substring(0, 3);

        return Float.parseFloat(majorMinorVersionString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMember getClusterMember(final int memberId) {
        if (!startAllInvoked) {
            LOGGER.warn("Cluster member group never started - cannot get member {}", memberId);

            return null;
        }

        LOGGER.debug("About to get cluster member {}", memberId);

        return getClusterMemberWrapper(memberId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownMember(final int... memberIds) {
        if (!startAllInvoked) {
            LOGGER.warn("Cluster member group never started - nothing to shutdown");

            return this;
        }

        for (final int memberId : memberIds) {
            LOGGER.info("About to shutdown cluster member {}", memberId);

            final DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);

            if (memberWrapper == null) {
                LOGGER.warn("Member with id {} did not exist in group - so cannot shut it down", memberId);
            } else {
                memberWrapper.shutdown();
            }
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownAll() {
        LOGGER.info("Restoring system properties back to their original state before member group started");

        System.setProperties(systemPropertiesBeforeStartAllInvoked);

        if (!startAllInvoked) {
            LOGGER.warn("Cluster member group never started - nothing to shutdown");

            return this;
        }

        callbackHandler.doBeforeShutdown();
        final int memberCount = memberFutures.size();

        LOGGER.debug("Shutting down {} cluster member(s) in group", memberCount);

        try {
            final long startTime = System.currentTimeMillis();

            for (final Future<DelegatingClusterMemberWrapper> task : memberFutures) {
                final DelegatingClusterMemberWrapper memberWrapper = task.get();

                memberWrapper.shutdown();
            }

            memberFutures.clear();
            final long shutdownDuration = System.currentTimeMillis() - startTime;

            LOGGER.info("___ Group of {} cluster member(s) shutdown in {}ms ___", memberCount, shutdownDuration);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        callbackHandler.doAfterShutdown();

        shutdownAllInvoked = true;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isAllShutdown() {
        return shutdownAllInvoked;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopMember(final int... memberIds) {
        if (!startAllInvoked) {
            LOGGER.warn("Cluster member group never started - nothing to do");

            return this;
        }

        for (final int memberId : memberIds) {
            LOGGER.info("About to stop cluster member with id {}", memberId);

            final DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);

            if (memberWrapper == null) {
                LOGGER.warn("Member with id {} did not exist in group - so cannot stop it", memberId);
            } else {
                memberWrapper.stop();
            }
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopAll() {
        if (!startAllInvoked) {
            LOGGER.warn("Cluster member group never started - nothing to stop");

            return this;
        }

        LOGGER.info("Stopping {} cluster member(s) in this group", memberFutures.size());

        try {
            for (final Future<DelegatingClusterMemberWrapper> task : memberFutures) {
                final DelegatingClusterMemberWrapper memberWrapper = task.get();

                memberWrapper.stop();
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }
}
