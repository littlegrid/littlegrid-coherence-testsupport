package org.testsupport.coherence;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;
import static org.testsupport.coherence.ClusterMemberGroupUtils.sleepAfterPerformingMemberShutdown;

/**
 * Cluster member group shutdown tests.
 */
public class ClusterMemberGroupShutdownTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void startAndShutdownSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeShutdown = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterShutdown = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToShutdown = 3;

        final ClusterMemberGroup memberGroup =
                newClusterMemberGroupBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSizeBeforeShutdown);
        assertThat(doesMemberExist(memberIdToShutdown), is(true));

        memberGroup.shutdownMember(memberIdToShutdown);
        sleepAfterPerformingMemberShutdown();
        assertThat(doesMemberExist(memberIdToShutdown), is(false));
        assertThatClusterIsExpectedSize(expectedClusterSizeAfterShutdown);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndShutdownNonExistentMemberOfGroup() {
        int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        int memberIdToShutdown = 12;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup =
                newClusterMemberGroupBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);
        assertThat(doesMemberExist(memberIdToShutdown), is(false));

        memberGroup.shutdownMember(memberIdToShutdown);
        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
    }
}
