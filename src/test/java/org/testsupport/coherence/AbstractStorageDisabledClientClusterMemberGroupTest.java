package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import com.tangosol.net.Member;
import org.junit.After;
import org.junit.Before;
import org.testsupport.common.AbstractTest;

import java.util.logging.Logger;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractStorageDisabledClientClusterMemberGroupTest extends AbstractTest {
    protected Logger logger = Logger.getLogger(AbstractStorageDisabledClientClusterMemberGroupTest.class.getName());

    protected static final String TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE = "coherence/testsupport-cache-config.xml";
    protected static final String EXTEND_CLIENT_CACHE_CONFIG_FILE = "coherence/testsupport-extend-client-cache-config.xml";

    protected static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;
    protected static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    protected static final int SMALL_TEST_CLUSTER_SIZE = 2;
    protected static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    protected static final int LARGE_TEST_CLUSTER_SIZE = 6;

    protected Cluster cluster;


    protected void assertThatClusterIsExpectedSize(final int expectedClusterSize) {
        assertThat(cluster.getMemberSet().size(), is(expectedClusterSize));
    }


    @Before
    public void beforeTest() {
        ClientUtils.setStorageDisabledClientSystemProperties(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
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

    protected boolean doesMemberExist(int specifiedMemberId) {
        for (Object object : cluster.getMemberSet()) {
            Member member = (Member) object;

            if (member.getId() == specifiedMemberId) {
                return true;
            }
        }

        return false;
    }
}
