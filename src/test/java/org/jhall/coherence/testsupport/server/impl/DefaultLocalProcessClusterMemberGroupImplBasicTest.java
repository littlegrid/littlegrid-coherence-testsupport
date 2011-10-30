package org.jhall.coherence.testsupport.server.impl;

import org.jhall.coherence.testsupport.server.ClusterMemberGroup;
import org.jhall.coherence.testsupport.server.ClusterMemberGroupConfig;
import org.jhall.coherence.testsupport.server.impl.util.PropertyContainer;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.jhall.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static org.jhall.coherence.testsupport.ServerFactory.shutdownClusterMemberGroups;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.HALTED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.NEVER_STARTED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.RUNNING;
import static org.jhall.coherence.testsupport.server.impl.AbstractDefaultLocalProcessClusterMemberGroupImplTest.StopShutdownMechanism.PERFORM_SHUTDOWN_ONLY;
import static org.junit.Assert.assertThat;

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
    public void neverStarted() {
        ClusterMemberGroup memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, null, false);

        assertThat(memberGroup.getStatus(), is(NEVER_STARTED));
        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void shutdownWhenNeverStarted() {
        ClusterMemberGroup memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, null, false);
        memberGroup.shutdownAll();

        assertThat(memberGroup.getStatus(), is(NEVER_STARTED));
        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void stopWhenNeverStarted() {
        ClusterMemberGroup memberGroup = createCacheServerGroup(SINGLE_TEST_CLUSTER_SIZE, null, null, null, false);
        memberGroup.stopAll();

        assertThat(memberGroup.getStatus(), is(NEVER_STARTED));
        assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @Test
    public void startAndShutdownSingleMemberGroup() {
        performStartOptionallyStopAndThenShutdownAndCheck(PERFORM_SHUTDOWN_ONLY, SINGLE_TEST_CLUSTER_SIZE);
    }

    @Test
    public void startInvokedTwice() {
        int numberOfServers = SINGLE_TEST_CLUSTER_SIZE;

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(numberOfServers);
            assertThat(memberGroup.getStatus(), is(RUNNING));
            assertThat(cluster.getMemberSet().size(), is(numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));

            memberGroup.startAll();
            assertThat(memberGroup.getStatus(), is(RUNNING));
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

            assertThat(memberGroup.getStatus(), is(RUNNING));
            assertThat(cluster.getMemberSet().size(), is(numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        } finally {
            shutdownClusterMemberGroups(memberGroup);

            assertThat(memberGroup, notNullValue());
            assertThat(memberGroup.getStatus(), is(HALTED));

            shutdownClusterMemberGroups(memberGroup);
            assertThat(memberGroup.getStatus(), is(HALTED));
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
