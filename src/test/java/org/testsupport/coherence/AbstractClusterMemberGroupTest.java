package org.testsupport.coherence;

import org.testsupport.common.AbstractTest;

import java.util.logging.Logger;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractClusterMemberGroupTest extends AbstractTest {
    protected Logger logger = Logger.getLogger(AbstractClusterMemberGroupTest.class.getName());

    protected static final String TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE = "coherence/inprocess-testsupport-cache-config.xml";
    protected static final String EXTEND_CLIENT_CACHE_CONFIG_FILE = "coherence/inprocess-testsupport-extend-client-cache-config.xml";

    protected static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;
    protected static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    protected static final int SMALL_TEST_CLUSTER_SIZE = 2;
    protected static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    protected static final int LARGE_TEST_CLUSTER_SIZE = 6;
}
