package org.littlegrid.coherence.testsupport;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Test;

import java.io.IOException;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

/**
 * Cluster member group example tests to show how to use the API.
 */
public class ClusterMemberGroupExampleTest extends AbstractClusterMemberGroupTest {
    private static final String KEY = "key";
    private static final String VALUE = "value";

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void exampleOfSimplestUse() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder().build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfStorageEnabledMembersWithCacheConfiguration() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2).setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfOneStorageEnabledExtendProxyMember() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfOneExtendProxyAndNoStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

        try {
            performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);

            fail("Exception was expected as there are no storage-enabled members");
        } catch (PortableException e) {
            // Coherence 3.6.x and above
        } catch (RuntimeException e) {
            // Coherence 3.5.x etc.
        }
    }

    @Test
    public void exampleOfExtendProxyWithSeparateStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfConfiguringExtendProxyWithSeparateStorageEnabledMembersThroughProperties() {
        /*
            These properties could be read from a file.
         */
        Properties properties = new Properties();
        properties.setProperty("StorageEnabledCount", "2");
        properties.setProperty("ExtendProxyCount", "1");
        properties.setProperty("CacheConfiguration", TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
        properties.setProperty("ClientCacheConfiguration", EXTEND_CLIENT_CACHE_CONFIG_FILE);

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder().setBuilderProperties(properties).build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfTwoAutonomousClustersEachWithOneStorageEnabledExtendProxyMember()
            throws IOException {

        Properties cluster1Properties = new Properties();
        cluster1Properties.load(this.getClass().getClassLoader()
                .getResourceAsStream("properties/memberGroup1.properties"));

        Properties cluster2Properties = new Properties();
        cluster2Properties.load(this.getClass().getClassLoader()
                .getResourceAsStream("properties/memberGroup2.properties"));

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties(cluster1Properties).build();

            memberGroup2 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties(cluster2Properties).build();

            performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    private void performSimplePutSizeGet(final String cacheName) {
        NamedCache cache = CacheFactory.getCache(cacheName);
        cache.put(KEY, VALUE);

        assertThat(cache.size(), is(1));
        assertThat((String) cache.get(KEY), is(VALUE));
    }
}
