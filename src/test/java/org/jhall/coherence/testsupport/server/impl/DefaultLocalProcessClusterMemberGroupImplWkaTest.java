package org.jhall.coherence.testsupport.server.impl;

import org.jhall.coherence.testsupport.server.ClusterMemberGroup;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.jhall.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static org.jhall.coherence.testsupport.ServerFactory.shutdownClusterMemberGroups;
import static org.jhall.coherence.testsupport.common.CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.junit.Assert.assertThat;

/**
 * Default local process cluster member group WKA tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplWkaTest extends
        AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void twoSmallMemberGroupsWithSameWka() {
        int numberOfServers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = (numberOfServers * 2) + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performTwoGroupsStartShutdownWithSpecifiedNumberInEachGroup(
                createCacheServerGroup(numberOfServers, TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                createCacheServerGroup(numberOfServers, TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                expectedClusterSize);
    }

    @Test
    public void twoSmallMemberGroupsWithDifferentWkas() {
        int numberOfServers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performTwoGroupsStartShutdownWithSpecifiedNumberInEachGroup(
                createCacheServerGroup(numberOfServers, TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                createCacheServerGroup(numberOfServers, TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE,
                        getAlternativeClusterConfiguration(), null, false),
                expectedClusterSize);
    }

    private void performTwoGroupsStartShutdownWithSpecifiedNumberInEachGroup(ClusterMemberGroup memberGroup1,
                                                                             ClusterMemberGroup memberGroup2,
                                                                             int expectedClusterSize) {

        try {
            memberGroup1.startAll();
            memberGroup2.startAll();
            assertThat(cluster.getMemberSet().size(), is(expectedClusterSize));
        } finally {
            shutdownClusterMemberGroups(memberGroup1, memberGroup2);

            assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        }
    }
}
