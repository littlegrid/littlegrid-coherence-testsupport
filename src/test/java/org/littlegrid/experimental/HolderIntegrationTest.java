package org.littlegrid.experimental;

import org.junit.Ignore;
import org.junit.Test;

/**
 * This is an experimental feature and is likely to change in the future!
 *
 * @since 2.11
 */
//TODO:
@Ignore
public class HolderIntegrationTest {
    @Test
    public void workInProgress() {
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
