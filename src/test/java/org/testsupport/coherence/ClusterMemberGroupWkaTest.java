package org.testsupport.coherence;

import org.junit.Test;

import java.util.logging.Logger;

import static java.lang.String.format;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;
import static org.testsupport.coherence.ClusterMemberGroupUtils.shutdownClusterMemberGroups;

/**
 * Cluster member group WKA tests.
 */
public class ClusterMemberGroupWkaTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    Logger logger = Logger.getLogger(ClusterMemberGroupWkaTest.class.getName());

    @Test
    public void twoSmallMemberGroupsWithSameWka() {
        int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = (numberOfMembers * 2) + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).build();

            memberGroup2 = newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).build();
            assertThatClusterIsExpectedSize(expectedClusterSize);
        } finally {
            shutdownClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    @Test
    public void twoSmallMemberGroupsWithDifferentWkas() {
        int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            ClusterMemberGroup.Builder builder =
                    newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers);
            memberGroup1 = builder.build();

            final int differentPort = builder.getWkaPort() + 20;
            logger.warning(format("A different WKA port of '%s' has been configured for a WKA test", differentPort));

            memberGroup2 = newClusterMemberGroupBuilder().setStorageEnabledCount(numberOfMembers).
                    setWkaPort(differentPort).build();
            assertThatClusterIsExpectedSize(expectedClusterSize);
        } finally {
            shutdownClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }
}
