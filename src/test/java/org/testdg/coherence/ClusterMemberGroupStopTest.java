package org.testdg.coherence;

import org.junit.After;
import org.junit.Test;
import org.testdg.coherence.support.ClusterMemberGroup;
import org.testdg.coherence.support.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Cluster member group stop tests.
 */
public class ClusterMemberGroupStopTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void startAndStopSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToStop = 3;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);
        ClusterMemberGroupUtils.sleepAfterPerformingMemberStop();
        assertThat(doesMemberExist(memberIdToStop), is(false));
        assertThatClusterIsExpectedSize(expectedClusterSizeAfterStop);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToStop = 12;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);
        assertThat(doesMemberExist(memberIdToStop), is(false));

        memberGroup.stopMember(memberIdToStop);
        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
    }

    @Test(expected = UnsupportedOperationException.class)
    public void attemptToStopMoreThanOneMemberWhichIsNotSupported() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(3).build().stopMember(1, 2);
    }
}
