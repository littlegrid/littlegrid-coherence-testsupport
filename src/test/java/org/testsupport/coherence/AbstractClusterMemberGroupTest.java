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
public abstract class AbstractClusterMemberGroupTest extends AbstractTest {
    protected static final String TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE = "coherence/testsupport-cache-config.xml";

    protected static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;
    protected static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    protected static final int SMALL_TEST_CLUSTER_SIZE = 2;
    protected static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    protected static final int LARGE_TEST_CLUSTER_SIZE = 6;
}
