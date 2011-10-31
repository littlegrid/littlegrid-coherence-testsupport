package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClientUtils;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.ClusterMemberGroupFactory;
import org.testsupport.coherence.support.common.CommonTestSupportConst;
import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;

import static org.testsupport.coherence.ClientUtils.setExtendClientSystemProperties;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createCacheServerGroup;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createExtendProxyServerGroup;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createSingleCompositeProxyAndCacheServer;
import static org.junit.Assert.fail;

/**
 * Default local process cluster member group Extend tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplExtendTest extends AbstractClusterMemberGroupTest {
    private static final String EXTEND_TEST_CACHE = "extend.test";

    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        ClusterMemberGroup extendProxyGroup = ClusterMemberGroupFactory.createExtendProxyServerGroup(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

        ClientUtils.setExtendClientSystemProperties(CommonTestSupportConst.EXTEND_CLIENT_CACHE_CONFIG_FILE);

        NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // A exception is expected for this test
        } finally {
            ClusterMemberGroupFactory.shutdownCacheFactoryThenClusterMemberGroups(extendProxyGroup);
        }
    }

    @Test
    public void extendProxyAndCacheServerStartedInSeparateGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup cacheServerGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            cacheServerGroup = ClusterMemberGroupFactory.createCacheServerGroup(numberOfCacheServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
            extendProxyGroup = ClusterMemberGroupFactory.createExtendProxyServerGroup(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            ClientUtils.setExtendClientSystemProperties(CommonTestSupportConst.EXTEND_CLIENT_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ClusterMemberGroupFactory.shutdownCacheFactoryThenClusterMemberGroups(cacheServerGroup, extendProxyGroup);
        }
    }

    @Test
    public void combinedExtendProxyAndCacheServer() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = ClusterMemberGroupFactory.createSingleCompositeProxyAndCacheServer(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            NamedCache cache = CacheFactory.getCache(EXTEND_TEST_CACHE);
            cache.put("any key", "single combined extend proxy and stored enabled member, so this will be cached");
        } finally {
            ClusterMemberGroupFactory.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }

    @Test
    @Ignore
    public void extendProxyWithCacheServersInSameGroup() {
//        ClusterMemberGroupFactory.createSingleExtendProxyWithCacheServerGroup()

    }
}
