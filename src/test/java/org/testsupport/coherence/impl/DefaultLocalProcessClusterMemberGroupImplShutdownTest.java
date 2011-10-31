package org.testsupport.coherence.impl;

import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupFactory.getSecondsToSleepAfterPerformingShutdown;

/**
 * Default local process cluster member group shutdown tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplShutdownTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndShutdownSmallMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY, SMALL_TEST_CLUSTER_SIZE);
    }

    @Test
    public void startAndShutdownSpecificMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 3;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY, numberOfServers,
                memberIdToShutdown, true, getSecondsToSleepAfterPerformingShutdown(),
                true, numberOfServers);
    }

    @Test
    public void startAndShutdownNonExistentMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 12;
        int expectedClusterSizeAfterAction = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY, numberOfServers,
                memberIdToShutdown, false, getSecondsToSleepAfterPerformingShutdown(),
                false, expectedClusterSizeAfterAction);
    }

    @Test
    public void runWithMoreThreadsAndShutdownMediumMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY, MEDIUM_TEST_CLUSTER_SIZE, 10);
    }
}
