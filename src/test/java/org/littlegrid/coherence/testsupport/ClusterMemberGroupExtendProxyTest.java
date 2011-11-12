package org.littlegrid.coherence.testsupport;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Test;

import static org.junit.Assert.fail;

/**
 * Cluster member group Extend tests.
 */
public class ClusterMemberGroupExtendProxyTest extends AbstractExtendClientClusterMemberGroupTest {
    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // A exception is expected for this test
        }
    }

    @Test
    public void extendProxyAndSeparateStorageEnabledMembersInSameGroup() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("any key", "separate extend proxy and stored enabled members, so this will be cached");
    }

    @Test
    public void extendProxyAndSeparateStorageEnabledMembersInDifferentGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup storageEnabledGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            storageEnabledGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfCacheServers).
                            setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

            extendProxyGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setExtendProxyCount(1)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                    .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

            NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(storageEnabledGroup, extendProxyGroup);
        }
    }

    @Test(expected = UnsupportedOperationException.class)
    public void multipleExtendProxiesWhichIsNotSupported() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();
    }
}
