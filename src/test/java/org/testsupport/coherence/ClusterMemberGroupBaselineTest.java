package org.testsupport.coherence;

import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupUtils.newBuilder;

/**
 * Cluster member group baseline tests.
 */
public class ClusterMemberGroupBaselineTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void neverStarted() {
        newBuilder().build();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void shutdownWhenNeverStarted() {
        newBuilder().build().shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void stopWhenNeverStarted() {
        newBuilder().build().stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownSingleMemberGroup() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndStopInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndShutdownWithKnownRequiredJarBeingExcluded() {
        newBuilder().setJarsToExcludeFromClassPath("junit-4.8.2.jar").build().startAll().shutdownAll();
    }
}
