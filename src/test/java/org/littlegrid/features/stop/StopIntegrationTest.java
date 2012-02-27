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

package org.littlegrid.features.stop;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupTestSupport;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.LARGE_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.SINGLE_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;
import static org.littlegrid.ClusterMemberGroupTestSupport.doesMemberExist;
import static org.littlegrid.ClusterMemberGroupTestSupport.sleepForSeconds;

/**
 * Cluster member group stop tests, for stopping all the members, specific members and
 * demonstrating failover for Extend clients when Extend proxy is stopped.
 */
public final class StopIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void startAndStopInvokedTwice() {
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);

        memberGroup.stopAll();
        memberGroup.stopAll();

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThatClusterIsExpectedSize(cluster, CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndStopLargeMemberGroup() {
        final int numberOfMembers = LARGE_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);

        memberGroup.stopAll();

        /*
            Wait longer because all of them are being stopped.
         */
        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());
        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());
        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThatClusterIsExpectedSize(cluster, CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
    }

    @Test
    public void startAndStopSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = expectedClusterSizeBeforeStop - 1;
        final int memberIdToStop = 3;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThat(doesMemberExist(cluster, memberIdToStop), is(false));
        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeAfterStop);
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int memberIdToStop = 12;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers).build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(false));

        memberGroup.stopMember(memberIdToStop);

        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);
    }

    @Test
    public void startAndStopSeveralMembersOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int[] memberIdsToStop = {1, 2};
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = expectedClusterSizeBeforeStop - memberIdsToStop.length;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(MEDIUM_TEST_CLUSTER_SIZE)
                .build();

        memberGroup.stopMember(1, 2);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSizeAfterStop);
    }

    @Test
    public void startAndStopExtendProxy() {
        final int numberOfExtendProxyMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int memberIdToStop = 1;
        final int expectedClusterSizeBeforeStop = numberOfExtendProxyMembers
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final int expectedClusterSizeAfterStop = expectedClusterSizeBeforeStop - 1;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
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
    public void startAndStopExtendProxyThatExtendClientIsConnectedTo() {
        final int numberOfExtendProxyMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int numberOfStorageEnabledMembers = SINGLE_TEST_CLUSTER_SIZE;

        final Properties additionalSystemProperties = new Properties();
        additionalSystemProperties.setProperty("tangosol.coherence.extend.address.2", "127.0.0.1");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.port.2", "20901");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.address.3", "127.0.0.1");
        additionalSystemProperties.setProperty("tangosol.coherence.extend.port.3", "20902");

        memberGroup = ClusterMemberGroupUtils.newBuilder()
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
