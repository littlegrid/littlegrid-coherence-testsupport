package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.support.common.CommonTestSupportConst;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createCacheServerGroup;
import static org.testsupport.coherence.ClusterMemberGroupFactory.shutdownClusterMemberGroups;
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
                createCacheServerGroup(numberOfServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                createCacheServerGroup(numberOfServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                expectedClusterSize);
    }

    @Test
    public void twoSmallMemberGroupsWithDifferentWkas() {
        int numberOfServers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = numberOfServers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        performTwoGroupsStartShutdownWithSpecifiedNumberInEachGroup(
                createCacheServerGroup(numberOfServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null, null, false),
                createCacheServerGroup(numberOfServers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE,
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
