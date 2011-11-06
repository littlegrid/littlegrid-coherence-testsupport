package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import com.tangosol.net.Member;
import org.junit.After;
import org.junit.Before;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
//import static org.testsupport.coherence.ClusterMemberGroupUtils.setStorageDisabledClientSystemProperties;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractStorageDisabledClientClusterMemberGroupTest extends AbstractClusterMemberGroupTest {
    protected static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;

    protected Cluster cluster;


    protected void assertThatClusterIsExpectedSize(final int expectedClusterSize) {
        cluster = CacheFactory.ensureCluster();

        assertThat(cluster.getMemberSet().size(), is(expectedClusterSize));
    }

    @After
    public void afterTest() {
        cluster = CacheFactory.ensureCluster();

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
