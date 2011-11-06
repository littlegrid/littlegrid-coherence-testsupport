package org.testsupport.coherence.impl;

import org.junit.Test;
import org.testsupport.coherence.ClusterMemberGroup;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Default cluster member group builder tests.
 */
public class DefaultClusterMemberGroupBuilderTest {
    private static final String DEFAULT_ADDRESS = "127.0.0.1";
    private static final int DEFAULT_PORT = 12501;

    @Test
    public void createAndCheckDefaults() {
        ClusterMemberGroup.Builder builder = new DefaultClusterMemberGroupBuilder();

        assertThat(builder.getStorageEnabledCount(), is(0));
        assertThat(builder.getExtendProxyCount(), is(0));
        assertThat(builder.getStorageEnabledExtendProxyCount(), is(0));
        assertThat(builder.getWkaAddress(), is(DEFAULT_ADDRESS));
        assertThat(builder.getWkaPort(), is(DEFAULT_PORT));
    }
}
