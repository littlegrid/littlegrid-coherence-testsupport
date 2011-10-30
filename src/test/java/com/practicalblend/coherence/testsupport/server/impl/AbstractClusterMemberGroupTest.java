package com.practicalblend.coherence.testsupport.server.impl;

import com.practicalblend.coherence.testsupport.common.AbstractTestSupportTest;

/**
 * Abstract base class for cluster member group tests.
 */
public abstract class AbstractClusterMemberGroupTest extends AbstractTestSupportTest {
    protected static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;
    protected static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    protected static final int SMALL_TEST_CLUSTER_SIZE = 2;
    protected static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    protected static final int LARGE_TEST_CLUSTER_SIZE = 4;
}
