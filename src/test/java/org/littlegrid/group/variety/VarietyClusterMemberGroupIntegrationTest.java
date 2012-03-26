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

package org.littlegrid.group.variety;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.EXTEND_CLIENT_CACHE_CONFIG_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_EXTEND_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;

/**
 * Variety of cluster member group tests, useful as a quick baseline check as a variety of
 * members are used..
 */
public final class VarietyClusterMemberGroupIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    private static final String KEY = "key";
    private static final String VALUE = "value";


    @Test
    public void storageEnabledMembersWithCacheConfiguration() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(2)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .buildAndConfigureForStorageDisabledClient();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void extendProxyWithSeparateStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .buildAndConfigureForExtendClient();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void clusterWithVarietyOfMembers() {
        final int storageEnabledCount = 2;
        final int extendProxyCount = 1;
        final int jmxMonitorCount = 1;
        final int expectedNumberOfMembers = storageEnabledCount + extendProxyCount + jmxMonitorCount;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(storageEnabledCount)
                .setExtendProxyCount(extendProxyCount)
                .setJmxMonitorCount(jmxMonitorCount)
                .buildAndConfigureForNoClient();

        assertThat(memberGroup.getStartedMemberIds().length, is(expectedNumberOfMembers));
    }

    private void performSimplePutSizeGet(final String cacheName) {
        final NamedCache cache = CacheFactory.getCache(cacheName);
        cache.put(KEY, VALUE);

        assertThat(cache.size(), is(1));
        assertThat((String) cache.get(KEY), is(VALUE));
    }
}
