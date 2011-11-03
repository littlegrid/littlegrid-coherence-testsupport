package org.testsupport.coherence;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractExtendClientClusterMemberGroupTest extends AbstractClusterMemberGroupTest {
    protected static final String EXTEND_CLIENT_CACHE_CONFIG_FILE = "coherence/testsupport-extend-client-cache-config.xml";
    protected static final String EXTEND_TEST_CACHE = "extend.test";
}
