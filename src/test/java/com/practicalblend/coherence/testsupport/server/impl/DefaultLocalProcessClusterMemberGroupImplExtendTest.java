package com.practicalblend.coherence.testsupport.server.impl;

import com.practicalblend.coherence.testsupport.ClientUtils;
import com.practicalblend.coherence.testsupport.ServerFactory;
import com.practicalblend.coherence.testsupport.common.CommonTestSupportConst;
import com.practicalblend.coherence.testsupport.server.ClusterMemberGroup;
import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;

import static com.practicalblend.coherence.testsupport.ClientUtils.setExtendClientSystemProperties;
import static com.practicalblend.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static com.practicalblend.coherence.testsupport.ServerFactory.createExtendProxyServerGroup;
import static com.practicalblend.coherence.testsupport.ServerFactory.createSingleCompositeProxyAndCacheServer;
import static org.junit.Assert.fail;

/**
 * Default local process cluster member group Extend tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplExtendTest extends AbstractClusterMemberGroupTest {
    private static final String EXTEND_TEST_CACHE = "extend.test";

    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        ClusterMemberGroup extendProxyGroup = ServerFactory.createExtendProxyServerGroup(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

        ClientUtils.setExtendClientSystemProperties(CommonTestSupportConst.EXTEND_CLIENT_CACHE_CONFIG_FILE);

        NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // A exception is expected for this test
        } finally {
            ServerFactory.shutdownCacheFactoryThenClusterMemberGroups(extendProxyGroup);
        }
    }

    @Test
    public void extendProxyAndCacheServerStartedInSeparateGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup cacheServerGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            cacheServerGroup = ServerFactory.createCacheServerGroup(numberOfCacheServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
            extendProxyGroup = ServerFactory.createExtendProxyServerGroup(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            ClientUtils.setExtendClientSystemProperties(CommonTestSupportConst.EXTEND_CLIENT_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ServerFactory.shutdownCacheFactoryThenClusterMemberGroups(cacheServerGroup, extendProxyGroup);
        }
    }

    @Test
    public void combinedExtendProxyAndCacheServer() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = ServerFactory.createSingleCompositeProxyAndCacheServer(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "single combined extend proxy and stored enabled member, so this will be cached");
        } finally {
            ServerFactory.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyWithCacheServersInSameGroup() {
//        ServerFactory.createSingleExtendProxyWithCacheServerGroup()

    }
}
