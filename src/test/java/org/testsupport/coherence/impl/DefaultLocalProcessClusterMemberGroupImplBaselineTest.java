package org.testsupport.coherence.impl;

import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupFactory.newBuilder;

/**
 * Default local process cluster member group basic tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplBaselineTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test(expected = IllegalStateException.class)
    public void constructWithNoPropertyContainer() {
        new DefaultLocalProcessClusterMemberGroupImpl(null, null);
    }

    @Test(expected = IllegalStateException.class)
    public void constructWithNoGroupConfig() {
        new DefaultLocalProcessClusterMemberGroupImpl(new PropertyContainer(), null);
    }

    @Test
    public void neverStarted() {
        memberGroup = newBuilder().build();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void shutdownWhenNeverStarted() {
        memberGroup = newBuilder().build().shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void stopWhenNeverStarted() {
        memberGroup = newBuilder().build().stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownSingleMemberGroup() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownInvokedTwice() {
        final int numberOfServers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = newBuilder().setNumberOfMembers(numberOfServers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndStopInvokedTwice() {
        final int numberOfServers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = newBuilder().setNumberOfMembers(numberOfServers).build().startAll();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndShutdownWithKnownRequiredJarBeingExcluded() {
        ClusterMemberGroupConfig memberGroupConfig = new ClusterMemberGroupConfig("junit-4.8.2.jar");

        memberGroup = newBuilder().setClusterMemberGroupConfig(memberGroupConfig).build().startAll().shutdownAll();
    }
}
