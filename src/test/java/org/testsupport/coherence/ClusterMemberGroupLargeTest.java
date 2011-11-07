package org.testsupport.coherence;

import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;

/**
 * Large cluster member group tests.
 */
public class ClusterMemberGroupLargeTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void startAndStopThenShutdownLargeMemberGroup() {
        final int numberOfMembers = LARGE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup =
                newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }
}
