/*
 * Copyright (c) 2010-2012 Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the LittleGrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Test;
import org.littlegrid.features.PretendServer;
import org.littlegrid.coherence.testsupport.impl.DefaultClusterMember;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.EXTEND_CLIENT_CACHE_CONFIG_FILE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.KNOWN_EXTEND_TEST_CACHE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.SINGLE_TEST_CLUSTER_SIZE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;

/**
 * Cluster member group example tests to show how to use the basic API.
 */
public final class ExampleClusterMemberGroupIntegrationTest
        extends AbstractAfterTestMemberGroupShutdownIntegrationTest {

    private static final String KEY = "key";
    private static final String VALUE = "value";

    @Test
    public void exampleOfSimplestUse() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfStorageEnabledMembersWithCacheConfiguration() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfOneStorageEnabledExtendProxyMember() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfExtendProxyWithSeparateStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfConfiguringExtendProxyWithSeparateStorageEnabledMembersThroughProperties() {
        /*
            These properties could be read from a file.
         */
        Properties properties = new Properties();
        properties.setProperty("StorageEnabledCount", "2");
        properties.setProperty("ExtendProxyCount", "1");
        properties.setProperty("CacheConfiguration", TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
        properties.setProperty("ClientCacheConfiguration", EXTEND_CLIENT_CACHE_CONFIG_FILE);

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setBuilderProperties(properties)
                .build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfTwoAutonomousClustersEachWithOneStorageEnabledExtendProxyMember() {
        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties("properties/memberGroup1.properties")
                    .build();

            memberGroup2 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties("properties/memberGroup2.properties")
                    .build();

            performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
        } finally {
            // Ensure they get shutdown
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    @Test
    public void exampleOfAdditionalSystemProperties() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setAdditionalSystemProperties("properties/additionalSystemProperties.properties")
                .build();

        assertThat(System.getProperty("SystemPropertyThatShouldHaveBeenSet"), notNullValue());
    }

    @Test
    public void exampleOfExtendingDefaultClusterMemberToUseLifeCycleMethods()
            throws Exception {

        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCustomConfiguredCount(numberOfMembers)
                .setCustomConfiguredClusterMemberInstanceClassName(
                        "org.littlegrid.coherence.testsupport.ExampleClusterMemberGroupIntegrationTest$PretendServerClusterMember")
                .build();

        assertThat(memberGroup.getStartedMemberIds().length, is(numberOfMembers));
    }

    @Test
    public void clusterWithVarietyOfMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2)
                .setExtendProxyCount(1)
                .setJmxMonitorCount(1)
                .build();
    }

    private void performSimplePutSizeGet(final String cacheName) {
        final NamedCache cache = CacheFactory.getCache(cacheName);
        cache.put(KEY, VALUE);

        assertThat(cache.size(), is(1));
        assertThat((String) cache.get(KEY), is(VALUE));
    }

    public static class PretendServerClusterMember extends DefaultClusterMember {
        private PretendServer server = new PretendServer();

        @Override
        public void doBeforeStart() {
            /*
                 At this point, Coherence hasn't been started in the other class loader - so functions
                 such as getting the member Id won't work (because it isn't running).

                 However, if you wanted to start out a JMS consumer or run a server then that is fine
                 because it will be in a different class loader.
             */

            System.out.println("Performing do before start - class loader: " + this.getClass().getClassLoader());
        }

        @Override
        public void doAfterStart() {
            server.start();
        }

        @Override
        public void doBeforeShutdown() {
            server.shutdown();
        }

        @Override
        public void doAfterShutdown() {
            /*
                 At this point, Coherence has been stopped - so functions such as getting the member Id
                 won't work (because it isn't running).

                 However, if you wanted to shutdown JMS consumer or shutdown a server then that is fine
                 because it will be in a different class loader.
             */

            System.out.println("Performing do after shutdown - class loader: " + this.getClass().getClassLoader());
        }
    }
}
