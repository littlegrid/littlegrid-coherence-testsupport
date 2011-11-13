package org.littlegrid.coherence.testsupport;

import org.junit.After;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Cluster member group shutdown tests.
 */
public class ClusterMemberGroupShutdownTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void startAndShutdownSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeShutdown = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterShutdown = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToShutdown = 3;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSizeBeforeShutdown);
        assertThat(doesMemberExist(memberIdToShutdown), is(true));

        memberGroup.shutdownMember(memberIdToShutdown);
        assertThat(doesMemberExist(memberIdToShutdown), is(false));
        assertThatClusterIsExpectedSize(expectedClusterSizeAfterShutdown);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndShutdownNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 12;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);
        assertThat(doesMemberExist(memberIdToShutdown), is(false));

        memberGroup.shutdownMember(memberIdToShutdown);
        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
    }

    @Test(expected = UnsupportedOperationException.class)
    public void attemptToShutdownMoreThanOneMemberWhichIsNotSupported() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(3).build().shutdownMember(1, 2);
    }
}