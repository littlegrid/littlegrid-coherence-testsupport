package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Test;

/**
 * Cluster member group Extend tests.
 */
public class ClusterMemberGroupStorageEnabledExtendTest extends AbstractExtendClientClusterMemberGroupTest {
    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void singleStorageEnabledExtendProxy() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");
    }

    @Test(expected = UnsupportedOperationException.class)
    public void multipleStorageEnabledExtendProxiesWhichIsNotSupported() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledExtendProxyCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();
    }
}
