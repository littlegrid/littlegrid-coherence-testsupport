/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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
 * Neither the name of the littlegrid nor the names of its contributors may
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

package org.littlegrid.group.extend_proxy;

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.InvocationService;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupBuildException;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.support.ExtendUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLIENT_OVERRIDE_CONFIGURATION_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.EXTEND_CLIENT_CACHE_CONFIGURATION_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.INVOCATION_SERVICE_NAME;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_EXTEND_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_MEMBER_NAME;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE;
import static org.littlegrid.support.ExtendUtils.getClusterSizeThatExtendClientIsConnectedTo;

/**
 * Cluster member group Extend tests.
 */
@Ignore
public final class ExtendProxyIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);

        try {
            cache.put("doesn't matter", "no storage enabled members, so will throw runtime exception");

            fail("Test should have failed due to no storage enabled members");
        } catch (PortableException e) {
            // Coherence 3.6.x and above
        } catch (RuntimeException e) {
            // Coherence 3.5.x etc.
        }
    }

    @Test
    public void extendProxyAndSeparateStorageEnabledMembersInSameGroup() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("any key", "separate extend proxy and stored enabled members, so this will be cached");

        assertThat(cache.size(), is(1));
    }

    @Test
    public void extendProxyAndSeparateStorageEnabledMembersInDifferentGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup storageEnabledGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            storageEnabledGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(numberOfCacheServers)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE)
                    .buildAndConfigureForNoClient();

            extendProxyGroup = ClusterMemberGroupUtils.newBuilder()
                    .setExtendProxyCount(1)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE)
                    .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                    .buildAndConfigureForExtendClient();

            final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(storageEnabledGroup, extendProxyGroup);
        }
    }

    @Test
    public void multipleExtendProxiesInSameGroup() {
        int numberOfExtendProxies = MEDIUM_TEST_CLUSTER_SIZE;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setExtendProxyCount(numberOfExtendProxies)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final InvocationService invocationService =
                (InvocationService) CacheFactory.getService(INVOCATION_SERVICE_NAME);

        final int clusterSize = getClusterSizeThatExtendClientIsConnectedTo(invocationService);
        assertThat(clusterSize, is(numberOfExtendProxies));
    }

    @Test
    public void extendProxyVersion() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final InvocationService invocationService =
                (InvocationService) CacheFactory.getService(INVOCATION_SERVICE_NAME);

        final String version = ExtendUtils.getExtendProxyMemberVersionThatClientIsConnectedTo(invocationService);
        assertThat(version, is(CacheFactory.VERSION));
    }

    @Test
    @Ignore
    public void extendClientOverrideForCoherence37AndAbove() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .setClientOverrideConfiguration(CLIENT_OVERRIDE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        assertThat(CacheFactory.getCluster().getLocalMember().getMemberName(), is(KNOWN_MEMBER_NAME));
    }

    @Test(expected = ClusterMemberGroupBuildException.class)
    @Ignore
    public void extendExtendProxyWithSamePortTwice() {
        //TODO: Move to identifiable exception once port in use is added as enum.
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .setClientOverrideConfiguration(CLIENT_OVERRIDE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForNoClient());
    }
}
