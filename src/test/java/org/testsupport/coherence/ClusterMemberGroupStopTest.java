package org.testsupport.coherence;

import org.junit.Ignore;
import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupFactory.getSecondsToSleepAfterPerformingStop;

/**
 * Cluster member group stop tests.
 */
public class ClusterMemberGroupStopTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    @Ignore
    public void startAndStopThenShutdownMediumMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, MEDIUM_TEST_CLUSTER_SIZE);
    }

    @Test
    @Ignore
    public void startAndStopSpecificMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 3;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, numberOfMembers,
                memberIdToStop, true, getSecondsToSleepAfterPerformingStop(),
                true, numberOfMembers);
    }

    @Test
    @Ignore
    public void startAndStopNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 12;
        int expectedClusterSizeAfterAction = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN, numberOfMembers,
                memberIdToStop, false, 0, false, expectedClusterSizeAfterAction);
    }
}
