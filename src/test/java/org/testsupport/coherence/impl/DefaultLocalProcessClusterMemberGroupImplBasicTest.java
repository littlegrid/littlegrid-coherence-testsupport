package org.testsupport.coherence.impl;

import org.junit.Test;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.ClusterMemberGroupFactory;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.testsupport.coherence.ClusterMemberGroup.Builder.Topology.STORAGE_ENABLED_ONLY;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createCacheServerGroup;
import static org.testsupport.coherence.ClusterMemberGroupFactory.newBuilder;
import static org.testsupport.coherence.ClusterMemberGroupFactory.shutdownClusterMemberGroups;

/**
 * Default local process cluster member group basic tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplBasicTest
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
    public void playWithFluentApi() {
        ClusterMemberGroup memberGroup =
                ClusterMemberGroupFactory.newBuilder().setTopology(STORAGE_ENABLED_ONLY).build()
                        .startAll().shutdownAll();
    }

    @Test
    public void neverStarted() {
        newBuilder().setMemberCount(SINGLE_TEST_CLUSTER_SIZE).build();

        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void shutdownWhenNeverStarted() {
        ClusterMemberGroup memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, null, false);
        memberGroup.shutdownAll();

        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void stopWhenNeverStarted() {
        ClusterMemberGroup memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, null, false);
        memberGroup.stopAll();

        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void startAndShutdownSingleMemberGroup() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = newBuilder()
                    .setMemberCount(numberOfMembers)
                    .build()
                    .startAll();

            assertThat(cluster.getMemberSet().size(), is(expectedClusterSize));
        } finally {
            shutdownClusterMemberGroups(memberGroup);

            assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        }
    }

    @Test
    public void startInvokedTwice() {
        int numberOfServers = SINGLE_TEST_CLUSTER_SIZE;

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(numberOfServers);
            assertThat(cluster.getMemberSet().size(), is(numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));

            memberGroup.startAll();
        } finally {
            shutdownClusterMemberGroups(memberGroup);
        }
    }

    @Test
    public void startAndShutdownInvokedTwice() {
        int numberOfServers = SINGLE_TEST_CLUSTER_SIZE;

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(numberOfServers);

            assertThat(cluster.getMemberSet().size(), is(numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        } finally {
            shutdownClusterMemberGroups(memberGroup);

            assertThat(memberGroup, notNullValue());

            shutdownClusterMemberGroups(memberGroup);
        }
    }

    @Test
    public void startAndShutdownWithKnownRequiredJarBeingExcluded() {
        ClusterMemberGroupConfig memberGroupConfig = new ClusterMemberGroupConfig("junit-4.8.2.jar");
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, memberGroupConfig);
        } finally {
            shutdownClusterMemberGroups(memberGroup);
        }
    }
}
