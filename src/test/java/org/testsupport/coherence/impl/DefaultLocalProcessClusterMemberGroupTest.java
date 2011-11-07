package org.testsupport.coherence.impl;

import org.junit.Ignore;
import org.junit.Test;
import org.testsupport.common.AbstractTest;

import java.util.Properties;

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
