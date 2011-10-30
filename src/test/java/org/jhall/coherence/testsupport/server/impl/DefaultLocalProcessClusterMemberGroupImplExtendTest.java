package org.jhall.coherence.testsupport.server.impl;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.jhall.coherence.testsupport.server.ClusterMemberGroup;
import org.junit.Ignore;
import org.junit.Test;

import static org.jhall.coherence.testsupport.ClientUtils.setExtendClientSystemProperties;
import static org.jhall.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static org.jhall.coherence.testsupport.ServerFactory.createExtendProxyServerGroup;
import static org.jhall.coherence.testsupport.ServerFactory.createSingleCompositeProxyAndCacheServer;
import static org.jhall.coherence.testsupport.ServerFactory.shutdownCacheFactoryThenClusterMemberGroups;
import static org.jhall.coherence.testsupport.common.CommonTestSupportConst.EXTEND_CLIENT_CACHE_CONFIG_FILE;
import static org.jhall.coherence.testsupport.common.CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.junit.Assert.fail;

/**
 * Default local process cluster member group Extend tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplExtendTest extends AbstractClusterMemberGroupTest {
    private static final String EXTEND_TEST_CACHE = "extend.test";

    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        ClusterMemberGroup extendProxyGroup = createExtendProxyServerGroup(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

        setExtendClientSystemProperties(EXTEND_CLIENT_CACHE_CONFIG_FILE);

        NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // A exception is expected for this test
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(extendProxyGroup);
        }
    }

    @Test
    public void extendProxyAndCacheServerStartedInSeparateGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup cacheServerGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            cacheServerGroup = createCacheServerGroup(numberOfCacheServers, TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
            extendProxyGroup = createExtendProxyServerGroup(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            setExtendClientSystemProperties(EXTEND_CLIENT_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(cacheServerGroup, extendProxyGroup);
        }
    }

    @Test
    public void combinedExtendProxyAndCacheServer() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createSingleCompositeProxyAndCacheServer(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "single combined extend proxy and stored enabled member, so this will be cached");
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyWithCacheServersInSameGroup() {
//        ServerFactory.createSingleExtendProxyWithCacheServerGroup()

    }
}
