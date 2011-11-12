package org.littlegrid.coherence.testsupport.impl;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Default local process cluster member group basic tests.
 */
@Ignore
public class DefaultLocalProcessClusterMemberGroupTest {
    @Test(expected = IllegalStateException.class)
    public void constructWithNoPropertyContainer() {
//        new DefaultLocalProcessClusterMemberGroup(null, null);
    }

    @Test(expected = IllegalStateException.class)
    public void constructWithNoGroupConfig() {
//        new DefaultLocalProcessClusterMemberGroup(new Properties(), null);
    }
}
