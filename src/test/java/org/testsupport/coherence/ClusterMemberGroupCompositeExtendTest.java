package org.testsupport.coherence;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;

import static org.junit.Assert.fail;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;
//import static org.testsupport.coherence.ClusterMemberGroupUtils.setExtendClientSystemProperties;
import static org.testsupport.coherence.ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups;

/**
 * Cluster member group Extend tests.
 */
public class ClusterMemberGroupCompositeExtendTest extends AbstractExtendClientClusterMemberGroupTest {
    @Test
    @Ignore
    public void noStorageEnabledMembersCannotStoreData() {
        ClusterMemberGroup extendProxyGroup = newClusterMemberGroupBuilder().setStorageEnabledExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

        throw new UnsupportedOperationException();
//        setExtendClientSystemProperties(EXTEND_CLIENT_CACHE_CONFIG_FILE);

//        NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
//
//        try {
//            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");
//
//            fail("Test should have failed due to no storage enabled members");
//        } catch (PortableException e) {
//            // A exception is expected for this test
//        } finally {
//            shutdownCacheFactoryThenClusterMemberGroups(extendProxyGroup);
//        }
    }

    @Test
    @Ignore
    public void extendProxyAndCacheServerStartedInSeparateGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup storageEnabledGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            storageEnabledGroup = newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfCacheServers).
                    setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

            extendProxyGroup = newClusterMemberGroupBuilder().setExtendProxyCount(1)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                    .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(storageEnabledGroup, extendProxyGroup);
        }
    }

    @Test
    @Ignore
    public void combinedExtendProxyAndCacheServer() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = newClusterMemberGroupBuilder().setStorageEnabledExtendProxyCount(1)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "single combined extend proxy and stored enabled member, so this will be cached");
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyWithCacheServersInSameGroup() {
//        ClusterMemberGroupUtils.createSingleExtendProxyWithCacheServerGroup()

    }
}
