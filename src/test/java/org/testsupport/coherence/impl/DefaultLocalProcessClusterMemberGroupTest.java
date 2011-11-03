package org.testsupport.coherence.impl;

import org.junit.Test;
import org.testsupport.common.AbstractTest;
import org.testsupport.common.lang.PropertyContainer;

/**
 * Default local process cluster member group basic tests.
 */
@Deprecated
public class DefaultLocalProcessClusterMemberGroupTest extends AbstractTest {
    @Test(expected = IllegalStateException.class)
    public void constructWithNoPropertyContainer() {
        new DefaultLocalProcessClusterMemberGroup(null, null);
    }

    @Test(expected = IllegalStateException.class)
    public void constructWithNoGroupConfig() {
        new DefaultLocalProcessClusterMemberGroup(new PropertyContainer(), null);
    }
}
