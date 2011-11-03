package org.testsupport.coherence.impl;

import org.junit.Test;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.lang.PropertyContainer;

import static org.testsupport.coherence.ClusterMemberGroupFactory.newBuilder;

/**
 * Default local process cluster member group basic tests.
 */
@Deprecated
public class DefaultLocalProcessClusterMemberGroupImplTest
        extends AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test(expected = IllegalStateException.class)
    public void constructWithNoPropertyContainer() {
        new DefaultLocalProcessClusterMemberGroupImpl(null, null);
    }

    @Test(expected = IllegalStateException.class)
    public void constructWithNoGroupConfig() {
        new DefaultLocalProcessClusterMemberGroupImpl(new PropertyContainer(), null);
    }
}
