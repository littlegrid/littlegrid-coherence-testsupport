package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClientUtils;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.support.common.CommonTestSupportConst;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import com.tangosol.net.Member;
import org.junit.After;
import org.junit.Before;

import java.util.Properties;
import java.util.Set;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.testsupport.coherence.ClientUtils.setStorageDisabledClientSystemProperties;
import static org.testsupport.coherence.ClusterMemberGroupFactory.createCacheServerGroup;
import static org.testsupport.coherence.ClusterMemberGroupFactory.getSecondsToSleepAfterPerformingStop;
import static org.testsupport.coherence.ClusterMemberGroupFactory.shutdownClusterMemberGroups;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.CLUSTER_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.DEFAULT_WKA_PORT;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCALPORT_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;
import static org.testsupport.coherence.impl.AbstractDefaultLocalProcessClusterMemberGroupImplTest.StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN;
import static org.junit.Assert.assertThat;

/**
 * Abstract default local process cluster member group tests.
 */
public abstract class AbstractDefaultLocalProcessClusterMemberGroupImplTest extends AbstractClusterMemberGroupTest {
    protected enum StopShutdownMechanism {PERFORM_STOP_THEN_SHUTDOWN, PERFORM_SHUTDOWN_ONLY}

    protected Cluster cluster;

    @Before
    public void beforeTest() {
        ClientUtils.setStorageDisabledClientSystemProperties(CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
        cluster = CacheFactory.ensureCluster();

        assertThat("Only storage disabled client is expected to be running before the cluster member tests start",
                cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
    }

    @After
    public void afterTest() {
        assertThat("Only storage disabled client is expected to be running after the cluster member tests have run",
                cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));

        CacheFactory.shutdown();
    }

    protected void performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism stopShutdownMechanism,
                                                                     int numberOfMembers) {

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(numberOfMembers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);

            performStartOptionallyStopAndThenShutdownAndCheck(stopShutdownMechanism, numberOfMembers, memberGroup);
        } finally {
            shutdownClusterMemberGroups(memberGroup);
        }
    }

    protected void performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism stopShutdownMechanism,
                                                                     int numberOfMembers,
                                                                     int numberOfThreadsInStartUpPool) {

        ClusterMemberGroupConfig groupConfig = new ClusterMemberGroupConfig();
        groupConfig.setNumberOfThreadsInStartUpPool(numberOfThreadsInStartUpPool);
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(numberOfMembers, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE, null,
                    groupConfig);

            performStartOptionallyStopAndThenShutdownAndCheck(stopShutdownMechanism, numberOfMembers, memberGroup);
        } finally {
            shutdownClusterMemberGroups();
        }
    }

    protected void performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism stopShutdownMechanism,
                                                                     int numberOfMembers,
                                                                     ClusterMemberGroup memberGroup) {

        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        try {
            assertThat(cluster.getMemberSet().size(), is(expectedClusterSize));

            if (stopShutdownMechanism == PERFORM_STOP_THEN_SHUTDOWN) {
                memberGroup.stopAll();

                sleepForSeconds(getSecondsToSleepAfterPerformingStop());

                assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
            }
        } finally {
            shutdownClusterMemberGroups(memberGroup);

            assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        }
    }

    protected void performStartShutdownOrStopOfSpecificMemberInGroupThenShutdown(StopShutdownMechanism stopShutdownMechanism,
                                                                                 int numberOfServersToStart,
                                                                                 int memberIdToShutdownOrStop,
                                                                                 boolean isMemberExpectedToExistBeforeAction,
                                                                                 int secondsToSleepAfterAction,
                                                                                 boolean expectedActionResult,
                                                                                 int expectedClusterSizeAfterAction) {

        ClusterMemberGroup memberGroup = null;
        try {
            if (true) {
                throw new UnsupportedOperationException();
            }
//            memberGroup = createCacheServerGroup(numberOfServersToStart, CommonTestSupportConst.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
//
//            assertThat(cluster.getMemberSet().size(), is(numberOfServersToStart + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
//            assertThat(checkIfSpecifiedMemberExists(cluster.getMemberSet(), memberIdToShutdownOrStop), is(isMemberExpectedToExistBeforeAction));
//
//            boolean actionResult;
//
//            if (stopShutdownMechanism == PERFORM_SHUTDOWN_ONLY) {
//                actionResult = memberGroup.shutdownMember(memberIdToShutdownOrStop);
//            } else {
//                actionResult = memberGroup.stopMember(memberIdToShutdownOrStop);
//            }
//
//            sleepForSeconds(secondsToSleepAfterAction);
//
//            assertThat(actionResult, is(expectedActionResult));
//            assertThat(cluster.getMemberSet().size(), is(expectedClusterSizeAfterAction));
        } finally {
            shutdownClusterMemberGroups(memberGroup);

            assertThat(cluster.getMemberSet().size(), is(CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));
        }
    }

    protected boolean checkIfSpecifiedMemberExists(Set<Member> memberSet, int specifiedMemberId) {
        for (Member member : memberSet) {
            if (member.getId() == specifiedMemberId) {
                return true;
            }
        }

        return false;
    }

    protected Properties getAlternativeClusterConfiguration() {
        final int portIncrement = 25;

        String currentPortString = System.getProperty(WKA_PORT_KEY, Integer.toString(DEFAULT_WKA_PORT));
        int differentPort = Integer.parseInt(currentPortString) + portIncrement;
        String differentPortString = Integer.toString(differentPort);

        logger.warn(format("A different WKA port of '%s' has been configured for a WKA test", differentPortString));

        Properties properties = new Properties();
        properties.setProperty(WKA_PORT_KEY, differentPortString);
        properties.setProperty(LOCALPORT_KEY, differentPortString);
        properties.setProperty(CLUSTER_KEY, "OtherTestLocalProcessCluster");

        return properties;
    }

    protected void sleepForSeconds(int seconds) {
        try {
            SECONDS.sleep(seconds);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}