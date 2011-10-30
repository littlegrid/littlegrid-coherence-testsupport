package com.practicalblend.coherence.testsupport.server.impl;

import org.junit.Test;

import static com.practicalblend.coherence.testsupport.ServerFactory.getSecondsToSleepAfterPerformingStop;
import static com.practicalblend.coherence.testsupport.server.impl.AbstractDefaultLocalProcessClusterMemberGroupImplTest.StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN;

/**
 * Default local process cluster member group stop tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplStopTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndStopThenShutdownMediumMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(PERFORM_STOP_THEN_SHUTDOWN, MEDIUM_TEST_CLUSTER_SIZE);
    }

    @Test
    public void startAndStopSpecificMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 3;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                PERFORM_STOP_THEN_SHUTDOWN, numberOfServers,
                memberIdToStop, true, getSecondsToSleepAfterPerformingStop(),
                true, numberOfServers);
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 12;
        int expectedClusterSizeAfterAction = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                PERFORM_STOP_THEN_SHUTDOWN, numberOfServers,
                memberIdToStop, false, 0, false, expectedClusterSizeAfterAction);
    }
}
