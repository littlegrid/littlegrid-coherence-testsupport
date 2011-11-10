package org.testsupport.coherence.impl;

import org.junit.Ignore;
import org.junit.Test;
import org.testsupport.coherence.ClusterMemberGroup;

/**
 * Default cluster member group builder tests.
 */
public class DefaultClusterMemberGroupBuilderTest {
    private static final String DEFAULT_ADDRESS = "127.0.0.1";
    private static final int DEFAULT_PORT = 21000;

    @Test
    @Ignore
    public void createAndCheckDefaults() {
        ClusterMemberGroup.Builder builder = new DefaultClusterMemberGroupBuilder();

//        assertThat(builder.getStorageEnabledCount(), is(0));
//        assertThat(builder.getExtendProxyCount(), is(0));
//        assertThat(builder.getStorageEnabledExtendProxyCount(), is(0));
//        assertThat(builder.getWkaAddress(), is(DEFAULT_ADDRESS));
//        assertThat(builder.getWkaPort(), is(DEFAULT_PORT));
    }
}
