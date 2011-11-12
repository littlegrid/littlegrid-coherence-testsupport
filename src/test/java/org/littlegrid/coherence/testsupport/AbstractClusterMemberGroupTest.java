package org.littlegrid.coherence.testsupport;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractClusterMemberGroupTest {
    protected static final String TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE = "coherence/littlegrid-test-cache-config.xml";
    protected static final String EXTEND_CLIENT_CACHE_CONFIG_FILE = "coherence/littlegrid-test-extend-client-cache-config.xml";
    protected static final String KNOWN_TEST_CACHE = "known-cache";
    protected static final String KNOWN_EXTEND_TEST_CACHE = "known-extend-cache";

    protected static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    protected static final int SMALL_TEST_CLUSTER_SIZE = 2;
    protected static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    protected static final int LARGE_TEST_CLUSTER_SIZE = 6;
}
