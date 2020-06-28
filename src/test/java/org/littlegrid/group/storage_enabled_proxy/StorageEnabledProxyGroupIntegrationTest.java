/*
 * Copyright (c) 2010-2020 Jonathan Hall.
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

package org.littlegrid.group.storage_enabled_proxy;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.InvocationService;
import com.tangosol.net.NamedCache;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.support.ExtendUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.EXTEND_CLIENT_CACHE_CONFIGURATION_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.INVOCATION_SERVICE_NAME;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_EXTEND_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE;

/**
 * Cluster member group Extend tests.
 */
@Ignore
public final class StorageEnabledProxyGroupIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void singleStorageEnabledExtendProxy() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledExtendProxyCount(1)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("key", "value");

        assertThat(cache.size(), is(1));
    }

    @Test
    public void multipleExtendProxiesInSameGroup() {
        final int numberOfStorageEnabledExtendProxies = MEDIUM_TEST_CLUSTER_SIZE;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledExtendProxyCount(numberOfStorageEnabledExtendProxies)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIGURATION_FILE)
                .buildAndConfigureForExtendClient();

        final InvocationService invocationService =
                (InvocationService) CacheFactory.getService(INVOCATION_SERVICE_NAME);

        final int clusterSize = ExtendUtils.getClusterSizeThatExtendClientIsConnectedTo(invocationService);

        assertThat(clusterSize, is(numberOfStorageEnabledExtendProxies));
    }
}
