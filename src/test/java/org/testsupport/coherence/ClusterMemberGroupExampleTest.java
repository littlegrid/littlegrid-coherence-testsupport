package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Cluster member group example tests to show how to use the API.
 */
public class ClusterMemberGroupExampleTest extends AbstractClusterMemberGroupTest {
    private static final String CACHE_NAME = "example-cache";
    private static final String KEY = "key";
    private static final String VALUE = "value";

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void simplestExample() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder().build();

        performSimplePutSizeGet(CACHE_NAME);
    }

    @Test
    public void storageEnabledMembersWithCacheConfigurationExample() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2).setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void storageEnabledExtendProxyExample() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void extendProxyExample() {
        throw new UnsupportedOperationException();
    }

    @Test
    public void extendProxyWithSeparateStorageEnabledExample() {
        throw new UnsupportedOperationException();
    }

    @Test
    public void twoAutonomousClustersWithExtendProxyAndWithSeparateStorageEnabledExample() {
        throw new UnsupportedOperationException();
    }

    private void performSimplePutSizeGet(final String cacheName) {
        NamedCache cache = CacheFactory.getCache(cacheName);
        cache.put(KEY, VALUE);

        assertThat(cache.size(), is(1));
        assertThat((String) cache.get(KEY), is(VALUE));
    }
}
