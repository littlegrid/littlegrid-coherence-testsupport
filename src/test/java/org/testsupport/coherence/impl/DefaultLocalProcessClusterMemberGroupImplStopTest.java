package org.testsupport.coherence.impl;

import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupFactory.getSecondsToSleepAfterPerformingStop;

/**
 * Default local process cluster member group stop tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplStopTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndStopThenShutdownMediumMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, MEDIUM_TEST_CLUSTER_SIZE);
    }

    @Test
    public void startAndStopSpecificMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 3;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, numberOfServers,
                memberIdToStop, true, getSecondsToSleepAfterPerformingStop(),
                true, numberOfServers);
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        int numberOfServers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 12;
        int expectedClusterSizeAfterAction = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, numberOfServers,
                memberIdToStop, false, 0, false, expectedClusterSizeAfterAction);
    }
}
