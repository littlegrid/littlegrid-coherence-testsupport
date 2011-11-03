package org.testsupport.coherence;

import org.junit.Ignore;
import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupFactory.getSecondsToSleepAfterPerformingShutdown;
import static org.testsupport.coherence.ClusterMemberGroupFactory.newBuilder;

/**
 * Cluster member group shutdown tests.
 */
public class ClusterMemberGroupShutdownTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndShutdownSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToShutdown = 3;

        final ClusterMemberGroup memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSizeBeforeStop);

        memberGroup.shutdownMember(memberIdToShutdown);
        sleepForSeconds(10);
        assertThatClusterIsExpectedSize(expectedClusterSizeAfterStop);

        memberGroup.shutdownAll();
    }

    @Test
    @Ignore
    public void startAndShutdownNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 12;
        int expectedClusterSizeAfterAction = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(
                StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY, numberOfMembers,
                memberIdToShutdown, false, getSecondsToSleepAfterPerformingShutdown(),
                false, expectedClusterSizeAfterAction);
    }
}
