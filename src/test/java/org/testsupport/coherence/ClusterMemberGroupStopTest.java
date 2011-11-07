package org.testsupport.coherence;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;
import static org.testsupport.coherence.ClusterMemberGroupUtils.sleepAfterPerformingMemberStop;

/**
 * Cluster member group stop tests.
 */
public class ClusterMemberGroupStopTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void startAndStopSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToStop = 3;

        final ClusterMemberGroup memberGroup =
                newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);
        sleepAfterPerformingMemberStop();
        assertThat(doesMemberExist(memberIdToStop), is(false));
        assertThatClusterIsExpectedSize(expectedClusterSizeAfterStop);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 12;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup =
                newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);
        assertThat(doesMemberExist(memberIdToStop), is(false));

        memberGroup.stopMember(memberIdToStop);
        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
    }
}
