package org.testsupport.coherence;

import org.junit.Test;

import java.util.logging.Logger;

import static java.lang.String.format;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newBuilder;
import static org.testsupport.coherence.ClusterMemberGroupUtils.shutdownClusterMemberGroups;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.DEFAULT_WKA_PORT;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

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
            memberGroup1 = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
            memberGroup2 = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
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
            memberGroup1 = newBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
            memberGroup2 = newBuilder().setNumberOfMembers(numberOfMembers).setWkaPort(getAlternativeWkaPort()).build().startAll();
            assertThatClusterIsExpectedSize(expectedClusterSize);
        } finally {
            shutdownClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    private int getAlternativeWkaPort() {
        final int portIncrement = 25;

        String currentPortString = System.getProperty(WKA_PORT_KEY, Integer.toString(DEFAULT_WKA_PORT));
        int differentPort = Integer.parseInt(currentPortString) + portIncrement;

        logger.warning(format("A different WKA port of '%s' has been configured for a WKA test", differentPort));

        return differentPort;
    }
}
