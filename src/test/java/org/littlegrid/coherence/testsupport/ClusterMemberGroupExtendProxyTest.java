/*
 * Copyright (c) 2011, Jonathan Hall.
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

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.InvocationService;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

/**
 * Cluster member group Extend tests.
 */
public class ClusterMemberGroupExtendProxyTest extends AbstractExtendClientClusterMemberGroupTest {
    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void noStorageEnabledMembersCannotStoreData() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

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
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("any key", "separate extend proxy and stored enabled members, so this will be cached");
    }

    @Test
    public void extendProxyAndSeparateStorageEnabledMembersInDifferentGroups() {
        int numberOfCacheServers = SMALL_TEST_CLUSTER_SIZE;

        ClusterMemberGroup storageEnabledGroup = null;
        ClusterMemberGroup extendProxyGroup = null;

        try {
            storageEnabledGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfCacheServers).
                            setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE).build();

            extendProxyGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setExtendProxyCount(1)
                    .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                    .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

            final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
            cache.put("any key", "storage enabled member(s) should be present, so this will be cached");
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(storageEnabledGroup, extendProxyGroup);
        }
    }

    @Test
    public void multipleExtendProxiesInSameGroup() {
        int numberOfExtendProxies = MEDIUM_TEST_CLUSTER_SIZE;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(numberOfExtendProxies)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        final InvocationService invocationService =
                (InvocationService) CacheFactory.getService(INVOCATION_SERVICE_NAME);

        final Map result = invocationService.query(new ClusterSizeInvocable(), null);
        assertThat(result.size(), is(1));

        final List<Integer> list = new ArrayList<Integer>(result.values());
        final int clusterSize = list.get(0);
        assertThat(clusterSize, is(numberOfExtendProxies));
    }

    @Test
    public void extendClientCacheConfigurationNotSpecified() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledExtendProxyCount(1)
                .build();

        // This should work, only a log message is logged when no Extend client cache config is specified
    }
}
