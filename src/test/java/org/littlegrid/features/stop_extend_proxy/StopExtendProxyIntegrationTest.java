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

package org.littlegrid.features.stop_extend_proxy;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupTestSupport;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.SINGLE_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;
import static org.littlegrid.ClusterMemberGroupTestSupport.doesMemberExist;
import static org.littlegrid.ClusterMemberGroupTestSupport.sleepForSeconds;

/**
 * Tests to stop Extend proxies.
 */
public class StopExtendProxyIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void startAndStopExtendProxy() {
        final int numberOfExtendProxyMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int memberIdToStop = 1;
        final int expectedClusterSizeBeforeStop = numberOfExtendProxyMembers
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final int expectedClusterSizeAfterStop = expectedClusterSizeBeforeStop - 1;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setExtendProxyCount(numberOfExtendProxyMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeAfterStop);
    }

    @Test
    @Ignore
    public void startAndStopExtendProxyThatExtendClientIsConnectedTo() {
        final int numberOfExtendProxyMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int numberOfStorageEnabledMembers = SINGLE_TEST_CLUSTER_SIZE;

        final Properties additionalSystemProperties = new Properties();
        additionalSystemProperties.setProperty("tangosol.coherence.extend.address.2", "127.0.0.1");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.port.2", "20901");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.address.3", "127.0.0.1");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.port.3", "20902");

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfStorageEnabledMembers)
                .setExtendProxyCount(numberOfExtendProxyMembers)
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setClientCacheConfiguration(
                        "coherence/littlegrid-test-extend-client-cache-config-with-multiple-remote-addresses.xml")
                .setAdditionalSystemProperties(additionalSystemProperties)
                .build();

        final int memberIdToStop = ClusterMemberGroupTestSupport.getExtendProxyMemberIdThatClientIsConnectedTo();

        memberGroup.stopMember(memberIdToStop);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        final int memberNowConnectedTo = ClusterMemberGroupTestSupport.getExtendProxyMemberIdThatClientIsConnectedTo();

        assertThat(memberNowConnectedTo != memberIdToStop, is(true));
    }
}
