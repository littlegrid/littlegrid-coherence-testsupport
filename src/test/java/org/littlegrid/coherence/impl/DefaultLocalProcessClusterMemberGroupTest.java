package org.littlegrid.coherence.impl;

import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.common.AbstractTest;

/**
 * Default local process cluster member group basic tests.
 */
@Ignore
public class DefaultLocalProcessClusterMemberGroupTest extends AbstractTest {
    @Test(expected = IllegalStateException.class)
    public void constructWithNoPropertyContainer() {
//        new DefaultLocalProcessClusterMemberGroup(null, null);
    }

    @Test(expected = IllegalStateException.class)
    public void constructWithNoGroupConfig() {
//        new DefaultLocalProcessClusterMemberGroup(new Properties(), null);
    }
}
