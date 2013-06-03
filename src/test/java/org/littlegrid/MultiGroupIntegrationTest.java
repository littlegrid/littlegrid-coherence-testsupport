package org.littlegrid;

import com.tangosol.net.CacheFactory;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.littlegrid.ClusterMemberGroup.Builder;

/**
 * Multi-group integration tests.
 */
public class MultiGroupIntegrationTest {
    public static class ClusterMemberGroupUtils2 {
        public static MultiGroupSwitcher newWhatever() {
            return new MultiGroupSwitcher() {
                @Override
                public void registerBuilder(String clusterName, Builder builder, GroupActivation groupActivation) {
                    throw new UnsupportedOperationException();
                }

                @Override
                public void buildAll() {
                    throw new UnsupportedOperationException();
                }

                @Override
                public ClusterMemberGroup active(String clusterName) {
                    throw new UnsupportedOperationException();
                }

                @Override
                public void shutdownAll() {
                    throw new UnsupportedOperationException();
                }
            };
        }
    }
    public static interface GroupActivation {

    }

    public static interface MultiGroupSwitcher {
        void registerBuilder(String clusterName,
                             Builder builder,
                             GroupActivation groupActivation);

        void buildAll();

        ClusterMemberGroup active(String clusterName);

        void shutdownAll();
    }

    @Test
    public void whatever() {
        final Builder builder1 = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1);

    }

    @Test
    public void twoAutonomousClusters() {
        final int numberOfMembers = 2;
        final int portOffset = 100;
        final int expectedClusterSize = numberOfMembers + 1; // Include this test which will join as a member

        ClusterMemberGroup memberGroupCluster1 = null;
        ClusterMemberGroup memberGroupCluster2 = null;

        try {
            // Build the first cluster (we won't actually be connecting to this one)
            memberGroupCluster1 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(numberOfMembers)
                    .buildAndConfigureForNoClient();

            // The second cluster will need to run on a different port to avoid clustering with
            // the first cluster
            final int member2WkaPort = memberGroupCluster1.getWkaPort() + portOffset;

            // Build the second cluster - we will join this cluster through this test by
            // asserting the cluster size
            memberGroupCluster2 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(numberOfMembers)
                    .setWkaPort(member2WkaPort)
                    .buildAndConfigureForStorageDisabledClient();

            // Check the size of the second cluster
            assertEquals(expectedClusterSize, CacheFactory.ensureCluster().getMemberSet().size());
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(
                    memberGroupCluster1, memberGroupCluster2);
        }
    }
}
