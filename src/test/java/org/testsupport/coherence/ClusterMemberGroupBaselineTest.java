package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;

import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;

/**
 * Cluster member group baseline tests.
 */
public class ClusterMemberGroupBaselineTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    @Ignore
    public void neverStarted() {
        newClusterMemberGroupBuilder().build();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    @Ignore
    public void shutdownWhenNeverStarted() {
        newClusterMemberGroupBuilder().build().shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    @Ignore
    public void stopWhenNeverStarted() {
        newClusterMemberGroupBuilder().build().stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownSingleMemberGroup() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        NamedCache cache = CacheFactory.getCache("test");
        cache.put("key", "value");

        memberGroup.shutdownAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    @Ignore
    public void startInvokedTwice() {
        throw new UnsupportedOperationException();
//        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
//        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
//
//        final ClusterMemberGroup memberGroup = newClusterMemberGroupBuilder()
//                .setStorageEnabledCount(numberOfMembers).build();
//        assertThatClusterIsExpectedSize(expectedClusterSize);
//
//        memberGroup.startAll();
//        assertThatClusterIsExpectedSize(expectedClusterSize);
//
//        memberGroup.shutdownAll();
//        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndShutdownInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = newClusterMemberGroupBuilder()
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

        final ClusterMemberGroup memberGroup = newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        assertThatClusterIsExpectedSize(expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.stopAll();
        assertThatClusterIsExpectedSize(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

        memberGroup.shutdownAll();
    }

    @Test
    @Ignore
    public void addTestWhereNoCacheConfigurationIsSpecifiedAndHaveFileCalledCoherenceCacheConfigToBePickedUp() {
    }

    @Test
    @Ignore
    public void addTestWhereSpecificCacheConfigurationIsUsedAndItHasSpecificSchemes() {

    }

    @Test
    @Ignore
    public void startAndShutdownWithKnownRequiredJarBeingExcluded() {
        final String jarToExclude = "junit-4.8.2.jar";

        newClusterMemberGroupBuilder().setJarsToExcludeFromClassPath(jarToExclude).build().shutdownAll();
    }
}
