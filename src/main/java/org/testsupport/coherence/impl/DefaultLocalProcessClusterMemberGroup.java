package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMember;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.LoggerPlaceHolder;

import java.io.File;
import java.net.MalformedURLException;
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
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT;

/**
 * Default local process cluster member group implementation.
 */
public class DefaultLocalProcessClusterMemberGroup implements ClusterMemberGroup {
    private final LoggerPlaceHolder logger =
            new LoggerPlaceHolder(DefaultLocalProcessClusterMemberGroup.class.getName());
    private boolean startInvoked;
    private Properties systemPropertiesBeforeStartInvoked;
    private Properties systemPropertiesToBeApplied;
    private List<Future<ClusterMemberDelegatingWrapper>> memberFutures =
            new ArrayList<Future<ClusterMemberDelegatingWrapper>>();
    private int numberOfMembers;
    private URL[] classPathUrls;
    private String clusterMemberInstanceClassName;
    private int numberOfThreadsInStartUpPool;


    /**
     * Constructor.
     *
     * @param systemPropertiesToBeApplied System systemPropertiesToBeApplied.
     */
    public DefaultLocalProcessClusterMemberGroup(final int numberOfMembers,
                                                 final Properties systemPropertiesToBeApplied,
                                                 final URL[] classPathUrls,
                                                 final String[] jarsToExcludeFromClassPath,
                                                 final String clusterMemberInstanceClassName,
                                                 final int numberOfThreadsInStartUpPool) {

        systemPropertiesBeforeStartInvoked = SystemUtils.snapshotSystemProperties();
        this.numberOfMembers = numberOfMembers;
        this.classPathUrls = classPathUrls;
        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;

        if (systemPropertiesToBeApplied == null) {
            throw new IllegalStateException("Property container cannot be null");
        }

        //TODO: Check incoming arguments

        this.systemPropertiesToBeApplied = systemPropertiesToBeApplied;
    }

    DefaultLocalProcessClusterMemberGroup() {
        systemPropertiesBeforeStartInvoked = SystemUtils.snapshotSystemProperties();
    }

    int merge(DefaultLocalProcessClusterMemberGroup memberGroup) {
        memberFutures.addAll(memberGroup.getMemberFutures());
        startInvoked = true;

        numberOfMembers = memberFutures.size();

        return memberFutures.size();
    }

    List<Future<ClusterMemberDelegatingWrapper>> getMemberFutures() {
        return memberFutures;
    }

    /**
     * Starts all the cluster members in the group.
     *
     * @return member group.
     */
    public ClusterMemberGroup startAll() {
        //TODO: Provide option to not stagger the start-up, e.g. for an additional member group to be
        // started to join an established cluster.
        if (startInvoked) {
            return this;
        }

        SystemUtils.applyToSystemProperties(systemPropertiesToBeApplied);
        startInvoked = true;
        outputStartAllMessages();

        try {
            List<Callable<ClusterMemberDelegatingWrapper>> tasks =
                    new ArrayList<Callable<ClusterMemberDelegatingWrapper>>(numberOfMembers);

            for (int i = 0; i < numberOfMembers; i++) {
                tasks.add(new ClusterMemberCallable(clusterMemberInstanceClassName, classPathUrls));
            }

            Callable<ClusterMemberDelegatingWrapper> taskForSeniorMember = tasks.remove(0);

            ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreadsInStartUpPool);

            logger.fine("About to establish a cluster using a single member initially");
            Future<ClusterMemberDelegatingWrapper> futureForSeniorMember = executorService.submit(taskForSeniorMember);
            futureForSeniorMember.get();

            logger.info("First cluster member up, starting any remaining members to join established cluster");
            List<Future<ClusterMemberDelegatingWrapper>> futuresForOtherMembers = executorService.invokeAll(tasks);

            memberFutures.add(futureForSeniorMember);
            memberFutures.addAll(futuresForOtherMembers);

            executorService.shutdown();

            List<Integer> memberIds = getStartedMemberIds();

            logger.info(format("Group of cluster member(s) started, member Ids: %s", memberIds));
        } catch (Exception e) {
            String message = format(
                    "Failed to start cluster member group - check Coherence system applied for misconfiguration: %s",
                    SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));

            System.setProperties(systemPropertiesBeforeStartInvoked);
            logger.severe(message);
            throw new IllegalStateException(message, e);
        }

        return this;
    }

    private void outputStartAllMessages() {
        final int oneMB = 1024 * 1024;

        logger.info(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
                numberOfMembers, numberOfThreadsInStartUpPool));

        logger.fine(format("Class path (after exclusions)..: %s", Arrays.deepToString(classPathUrls)));
        logger.fine(format("Current Coherence applied...: %s", SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));
        logger.info(format("Server system applied to set: %s", systemPropertiesToBeApplied));
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
                Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                ClusterMemberDelegatingWrapper memberWrapper = task.get();

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
            List<Integer> memberIds = new ArrayList<Integer>();

            for (int i = 0; i < memberFutures.size(); i++) {
                Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                ClusterMemberDelegatingWrapper memberWrapper = task.get();
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
            logger.warning(format("Cluster member group never started - cannot get member '%s'", memberId));

            return null;
        }

        logger.fine(format("About to get cluster member '%d'", memberId));

        return getClusterMemberWrapper(memberId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownMember(final int... memberIds) {
        if (!startInvoked) {
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Shutting down multiple members is not supported currently");
        }

        int memberId = memberIds[0];

        logger.info(format("About to shutdown cluster member '%d'", memberId));

        ClusterMemberDelegatingWrapper memberWrapper = getClusterMemberWrapper(memberId);

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
        if (!startInvoked) {
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }

        System.setProperties(systemPropertiesBeforeStartInvoked);

        //TODO: this should report the entire cluster size, not just this instance of the CMG
        logger.info(format("Shutting down '%d' cluster member(s) in group", numberOfMembers));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                ClusterMemberDelegatingWrapper memberWrapper = task.get();
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
    public ClusterMemberGroup stopMember(int... memberIds) {
        if (!startInvoked) {
            logger.warning("Cluster member group never started - nothing to ");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Stopping multiple members is not supported currently");
        }

        int memberId = memberIds[0];

        logger.info(format("About to stop cluster member '%d'", memberId));

        ClusterMemberDelegatingWrapper memberWrapper = getClusterMemberWrapper(memberId);

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
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }


        //TODO: this should report the entire cluster size, not just this instance of the CMG
        logger.info(format("Stopping '%d' cluster member(s) in this group", numberOfMembers));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                Future<ClusterMemberDelegatingWrapper> task = memberFutures.get(i);

                ClusterMemberDelegatingWrapper memberWrapper = task.get();
                memberWrapper.stop();
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }
}
