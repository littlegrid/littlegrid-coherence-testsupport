package com.practicalblend.coherence.testsupport.server.impl;

import org.junit.Test;

import static com.practicalblend.coherence.testsupport.ServerFactory.getSecondsToSleepAfterPerformingShutdown;
import static com.practicalblend.coherence.testsupport.server.impl.AbstractDefaultLocalProcessClusterMemberGroupImplTest.StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY;

/**
 * Default local process cluster member group shutdown tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplShutdownTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndShutdownSmallMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(PERFORM_SHUTDOWN_ONLY, SMALL_TEST_CLUSTER_SIZE);
    }

    @Test
    public void startAndShutdownSpecificMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 3;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                PERFORM_SHUTDOWN_ONLY, numberOfServers,
                memberIdToShutdown, true, getSecondsToSleepAfterPerformingShutdown(),
                true, numberOfServers);
    }

    @Test
    public void startAndShutdownNonExistentMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 12;
        int expectedClusterSizeAfterAction = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                PERFORM_SHUTDOWN_ONLY, numberOfServers,
                memberIdToShutdown, false, getSecondsToSleepAfterPerformingShutdown(),
                false, expectedClusterSizeAfterAction);
    }

    @Test
    public void runWithMoreThreadsAndShutdownMediumMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(PERFORM_SHUTDOWN_ONLY, MEDIUM_TEST_CLUSTER_SIZE, 10);
    }
}
