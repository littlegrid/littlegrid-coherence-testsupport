package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Test;

/**
 * Cluster member group baseline tests.
 */
public class ClusterMemberGroupBaselineTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void startAndShutdownSingleMemberGroup() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        NamedCache cache = CacheFactory.getCache("test");
        cache.put("key", "value");

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void simpleMemberGroupWithCacheConfigurationAndKnownCache() {
        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

        NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);
        cache.put("key", "value");

        memberGroup.shutdownAll();
    }

    @Test(expected = IllegalArgumentException.class)
    public void simpleMemberGroupWithCacheConfigurationAndUnknownCache() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

            NamedCache cache = CacheFactory.getCache("unknown-cache-this-will-not-be-found-in-cache-configuration");
            cache.put("key", "value");
        } finally {
            memberGroup.shutdownAll();
        }
    }

    @Test
    public void startAndShutdownInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndStopInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.stopAll();

        ClusterMemberGroupUtils.sleepAfterPerformingMemberStop();

        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndShutdownWithKnownRequiredJarBeingExcluded() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final String jarToExclude = "junit-4.8.2.jar";

        ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setJarsToExcludeFromClassPath(jarToExclude)
                .build();

        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.shutdownAll();
    }
}
