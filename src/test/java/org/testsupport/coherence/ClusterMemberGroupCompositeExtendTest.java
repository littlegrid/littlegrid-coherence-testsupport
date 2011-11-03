package org.testsupport.coherence;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.fail;
import static org.testsupport.coherence.ClusterMemberGroup.Builder.Topology.COMPOSITE_STORAGE_ENABLED_PROXY;
import static org.testsupport.coherence.ClusterMemberGroup.Builder.Topology.EXTEND_PROXY_ONLY;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;
import static org.testsupport.coherence.ClusterMemberGroupUtils.setExtendClientSystemProperties;

/**
 * Cluster member group Extend tests.
 */
public class ClusterMemberGroupCompositeExtendTest extends AbstractExtendClientClusterMemberGroupTest {
    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        ClusterMemberGroup extendProxyGroup = newClusterMemberGroupBuilder().setTopology(EXTEND_PROXY_ONLY)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build().startAll();

        setExtendClientSystemProperties(EXTEND_CLIENT_CACHE_CONFIG_FILE);

        NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // A exception is expected for this test
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(extendProxyGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyAndCacheServerStartedInSeparateGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup cacheServerGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            cacheServerGroup = newClusterMemberGroupBuilder().setNumberOfMembers(numberOfCacheServers).
                    setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build().startAll();
            extendProxyGroup = newClusterMemberGroupBuilder().setTopology(EXTEND_PROXY_ONLY).
                    setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build().startAll();

            setExtendClientSystemProperties(EXTEND_CLIENT_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(cacheServerGroup, extendProxyGroup);
        }
    }

    @Test
    @Ignore
    public void combinedExtendProxyAndCacheServer() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = newClusterMemberGroupBuilder().setTopology(COMPOSITE_STORAGE_ENABLED_PROXY)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build().startAll();

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "single combined extend proxy and stored enabled member, so this will be cached");
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyWithCacheServersInSameGroup() {
//        ClusterMemberGroupUtils.createSingleExtendProxyWithCacheServerGroup()

    }
}
