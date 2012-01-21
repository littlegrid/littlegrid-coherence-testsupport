/*
 * Copyright (c) 2011, Jonathan Hall.
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
 * Neither the name of the LittleGrid nor the names of its contributors may
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

package org.littlegrid.coherence.testsupport.impl;

import org.littlegrid.coherence.testsupport.ClusterMember;
import org.littlegrid.coherence.testsupport.ClusterMemberGroup;
import org.littlegrid.common.LoggerPlaceHolder;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static java.lang.String.format;
import static org.littlegrid.coherence.testsupport.SystemPropertyConst.TANGOSOL_COHERENCE_DOT;

/**
 * Default local process cluster member group implementation.
 */
public final class DefaultLocalProcessClusterMemberGroup implements ClusterMemberGroup {
    private final LoggerPlaceHolder logger =
            new LoggerPlaceHolder(DefaultLocalProcessClusterMemberGroup.class.getName());

    private final List<Future<ClusterMemberDelegatingWrapper>> memberFutures =
            new ArrayList<Future<ClusterMemberDelegatingWrapper>>();

    private boolean startInvoked;
    private Properties systemPropertiesBeforeStartInvoked;
    private Properties systemPropertiesToBeApplied;
    private int numberOfMembers;
    private URL[] classPathUrls;
    private String clusterMemberInstanceClassName;
    private int numberOfThreadsInStartUpPool;


    /**
     * Constructor.
     *
     * @param numberOfMembers                Number of members.
     * @param systemPropertiesToBeApplied    System properties to be applied.
     * @param classPathUrls                  Class path.
     * @param clusterMemberInstanceClassName Class name of cluster member instance.
     * @param numberOfThreadsInStartUpPool   Number of threads in start-up pool.
     */
    public DefaultLocalProcessClusterMemberGroup(final int numberOfMembers,
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
            throw new IllegalArgumentException("No class path URLs specified - will not be able to necessary classes");
        }

        if (clusterMemberInstanceClassName == null || clusterMemberInstanceClassName.trim().length() == 0) {
            throw new IllegalArgumentException("No cluster member instance class name, cannot setup cluster");
        }

        if (numberOfThreadsInStartUpPool < 1) {
            throw new IllegalArgumentException("Invalid number of threads specified for start-up pool, cannot start");
        }

        this.numberOfMembers = numberOfMembers;
        this.classPathUrls = classPathUrls;
        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;
        this.systemPropertiesToBeApplied = systemPropertiesToBeApplied;

        systemPropertiesBeforeStartInvoked = SystemUtils.snapshotSystemProperties();
    }

    /**
     * Constructor with reduced scope.
     */
    DefaultLocalProcessClusterMemberGroup() {
        systemPropertiesBeforeStartInvoked = SystemUtils.snapshotSystemProperties();
    }

    /**
     * Reduced scope method to merge in a cluster member group with this cluster member group.
     *
     * @param memberGroup Cluster member group to be merged.
     * @return new size of combined member group.
     */
    int merge(final ClusterMemberGroup memberGroup) {
        final DefaultLocalProcessClusterMemberGroup defaultClusterMemberGroup =
                (DefaultLocalProcessClusterMemberGroup) memberGroup;

        memberFutures.addAll(defaultClusterMemberGroup.getMemberFutures());
        startInvoked = true;

        numberOfMembers = memberFutures.size();

        return memberFutures.size();
    }

    /**
     * Reduced scope method to get this members list of futures, the references to the
     * cluster members.
     *
     * @return list of member's futures.
     */
    List<Future<ClusterMemberDelegatingWrapper>> getMemberFutures() {
        return memberFutures;
    }

    /**
     * Starts all the cluster members in the group.
     *
     * @return member group.
     */
    public ClusterMemberGroup startAll() {
        //TODO: littlegrid#10 Provide option to not stagger the start-up, e.g. for an additional member group to be
        // started to join an established cluster.
        if (startInvoked) {
            return this;
        }

        SystemUtils.applyToSystemProperties(systemPropertiesToBeApplied);
        startInvoked = true;
        outputStartAllMessages();

        try {
            final List<Callable<ClusterMemberDelegatingWrapper>> tasks =
                    new ArrayList<Callable<ClusterMemberDelegatingWrapper>>(numberOfMembers);

            for (int i = 0; i < numberOfMembers; i++) {
                tasks.add(new ClusterMemberCallable(clusterMemberInstanceClassName, classPathUrls));
            }

            final Callable<ClusterMemberDelegatingWrapper> taskForSeniorMember = tasks.remove(0);

            final ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreadsInStartUpPool);

            logger.debug("About to establish a cluster using a single member initially");
            final Future<ClusterMemberDelegatingWrapper> futureForSeniorMember =
                    executorService.submit(taskForSeniorMember);

            futureForSeniorMember.get();

            logger.info("First cluster member up, starting any remaining members to join established cluster");
            final List<Future<ClusterMemberDelegatingWrapper>> futuresForOtherMembers =
                    executorService.invokeAll(tasks);

            memberFutures.add(futureForSeniorMember);
            memberFutures.addAll(futuresForOtherMembers);

            executorService.shutdown();

            logger.info(format("Group of cluster member(s) started, member Ids: %s", getStartedMemberIds()));
        } catch (Exception e) {
            final String message = format(
                    "Failed to start cluster member group - check Coherence system applied for misconfiguration: %s",
                    SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));

            System.setProperties(systemPropertiesBeforeStartInvoked);
            logger.error(message);
            throw new IllegalStateException(message, e);
        }

        return this;
    }

    private void outputStartAllMessages() {
        final int oneMB = 1024 * 1024;

        logger.info(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
                numberOfMembers, numberOfThreadsInStartUpPool));

        logger.debug(format("Class path (after exclusions)..: %s", Arrays.deepToString(classPathUrls)));
        logger.debug(format("Current Coherence properties...: %s",
                SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));
        logger.info(format("Coherence properties to be set.: %s", systemPropertiesToBeApplied));
        logger.info(format("Max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private ClusterMemberDelegatingWrapper getClusterMemberWrapper(final int memberId) {
        if (!startInvoked) {
            throw new IllegalStateException("Cluster member group never started");
        }

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                final Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                final ClusterMemberDelegatingWrapper memberWrapper = task.get();

                if (memberWrapper.getLocalMemberId() == memberId) {
                    return memberWrapper;
                }
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Integer> getStartedMemberIds() {
        try {
            final List<Integer> memberIds = new ArrayList<Integer>();

            for (int i = 0; i < memberFutures.size(); i++) {
                final Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                final ClusterMemberDelegatingWrapper memberWrapper = task.get();
                memberIds.add(memberWrapper.getLocalMemberId());
            }

            return memberIds;
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMember getClusterMember(final int memberId) {
        if (!startInvoked) {
            logger.warn(format("Cluster member group never started - cannot get member '%s'", memberId));

            return null;
        }

        logger.debug(format("About to get cluster member '%d'", memberId));

        return getClusterMemberWrapper(memberId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownMember(final int... memberIds) {
        if (!startInvoked) {
            logger.warn("Cluster member group never started - nothing to shutdown");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Shutting down multiple members is not supported currently");
        }

        final int memberId = memberIds[0];

        logger.info(format("About to shutdown cluster member '%d'", memberId));

        final ClusterMemberDelegatingWrapper memberWrapper = getClusterMemberWrapper(memberId);

        if (memberWrapper != null) {
            memberWrapper.shutdown();
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownAll() {
        logger.info("Resetting system properties to before member group started");

        System.setProperties(systemPropertiesBeforeStartInvoked);

        if (!startInvoked) {
            logger.warn("Cluster member group never started - nothing to shutdown");

            return this;
        }

        logger.info(format("Shutting down '%d' cluster member(s) in group", numberOfMembers));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                final Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                final ClusterMemberDelegatingWrapper memberWrapper = task.get();

                memberWrapper.shutdown();
            }

            memberFutures.clear();

            logger.info("Group of cluster member(s) shutdown");
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopMember(final int... memberIds) {
        if (!startInvoked) {
            logger.warn("Cluster member group never started - nothing to do");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Stopping multiple members is not supported currently");
        }

        final int memberId = memberIds[0];

        logger.info(format("About to stop cluster member with id '%d'", memberId));

        final ClusterMemberDelegatingWrapper memberWrapper = getClusterMemberWrapper(memberId);

        if (memberWrapper != null) {
            memberWrapper.stop();
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopAll() {
        if (!startInvoked) {
            logger.warn("Cluster member group never started - nothing to shutdown");

            return this;
        }

        logger.info(format("Stopping '%d' cluster member(s) in this group", numberOfMembers));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                final Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                final ClusterMemberDelegatingWrapper memberWrapper = task.get();

                memberWrapper.stop();
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }
}
