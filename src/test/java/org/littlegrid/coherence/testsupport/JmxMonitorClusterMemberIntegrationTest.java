package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import org.junit.After;
import org.junit.Test;

import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;

/**
 * Custom configured member tests.
 */
public final class JmxMonitorClusterMemberIntegrationTest
        extends AbstractStorageDisabledClientClusterMemberGroupIntegrationTest {

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void jmxMonitorClusterMember() {
        final int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setJmxMonitorCount(numberOfMembers)
                .build();

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);
    }
}
