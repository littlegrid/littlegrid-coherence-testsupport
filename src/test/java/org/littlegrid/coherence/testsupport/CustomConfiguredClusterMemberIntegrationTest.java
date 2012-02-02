package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import com.tangosol.net.RequestPolicyException;
import org.junit.After;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;

/**
 * Custom configured member tests.
 */
public class CustomConfiguredClusterMemberIntegrationTest
        extends AbstractStorageDisabledClientClusterMemberGroupIntegrationTest {

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test(expected = RequestPolicyException.class)
    public void storageDisabledClusterMember() {
        final int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setCustomConfiguredCount(numberOfMembers)
                .build();

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);

        final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);
        cache.put("key", "value");

        assertThat(cache.size(), is(1));
    }
}
