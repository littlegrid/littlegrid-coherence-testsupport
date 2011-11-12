package org.littlegrid.coherence;

import org.junit.Test;

/**
 * Large cluster member group tests.
 */
public class ClusterMemberGroupLargeTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void startAndStopThenShutdownLargeMemberGroup() {
        final int numberOfMembers = LARGE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();

        /*
            Wait longer because all of them are being stopped.
         */
        ClusterMemberGroupUtils.sleepAfterPerformingMemberStop();
        ClusterMemberGroupUtils.sleepAfterPerformingMemberStop();
        ClusterMemberGroupUtils.sleepAfterPerformingMemberStop();

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }
}
