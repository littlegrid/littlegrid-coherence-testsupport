package org.littlegrid.features.holder;

import org.junit.Test;
import org.littlegrid.ClusterMemberGroupHolder;
import org.littlegrid.ClusterMemberGroupHolderUtils;

/**
 */
//TODO:
public class HolderIntegrationTest {
    @Test
    public void whatever() {
        final ClusterMemberGroupHolder groupHolder1 = ClusterMemberGroupHolderUtils
                .newBuilder(ClusterMemberGroupHolder.DefaultClusterMemberGroupHolder.class)
                .build();

        final ClusterMemberGroupHolder groupHolder2 = ClusterMemberGroupHolderUtils
                .newBuilder(ClusterMemberGroupHolder.DefaultClusterMemberGroupHolder.class)
                .build();


        ClusterMemberGroupHolderUtils.shutdownClusterMemberGroupHolders(groupHolder1);
        ClusterMemberGroupHolderUtils.shutdownClusterMemberGroupHolders(groupHolder2);


        final ClusterMemberGroupHolder groupHolder3 = ClusterMemberGroupHolderUtils
                .newBuilder(ClusterMemberGroupHolder.DefaultClusterMemberGroupHolder.class)
                .build();

        ClusterMemberGroupHolderUtils.shutdownClusterMemberGroupHolders(groupHolder3);

        final ClusterMemberGroupHolder groupHolder4 = ClusterMemberGroupHolderUtils
                .newBuilder(ClusterMemberGroupHolder.DefaultClusterMemberGroupHolder.class)
                .build();

        ClusterMemberGroupHolderUtils.shutdownClusterMemberGroupHolders(groupHolder4);
    }
}
