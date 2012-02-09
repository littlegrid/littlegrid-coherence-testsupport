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

package org.littlegrid;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import org.junit.After;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.SINGLE_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;
import static org.littlegrid.ClusterMemberGroupTestSupport.doesMemberExist;
import static org.littlegrid.ClusterMemberGroupTestSupport.sleepForSeconds;

/**
 * Cluster member group stop tests.
 */
public final class StopClusterMemberGroupIntegrationTest
        extends AbstractStorageDisabledClientClusterMemberGroupIntegrationTest {

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void startAndStopSpecificMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int expectedClusterSizeBeforeStop = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
        final int expectedClusterSizeAfterStop = (numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP) - 1;
        final int memberIdToStop = 3;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThat(doesMemberExist(cluster, memberIdToStop), is(false));
        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeAfterStop);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndStopNonExistentMemberOfGroup() {
        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int memberIdToStop = 12;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(false));

        memberGroup.stopMember(memberIdToStop);

        // No need to wait - it never existed
        assertThatClusterIsExpectedSize(cluster, expectedClusterSize);

        memberGroup.shutdownAll();
    }

    @Test
    public void startAndStopExtendProxyMemberOfGroup() {
        final int numberOfExtendProxyMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int numberOfStorageEnabledMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int memberIdToStop = 2;
        final int expectedClusterSizeBeforeStop = numberOfExtendProxyMembers + numberOfStorageEnabledMembers
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        final int expectedClusterSizeAfterStop = numberOfStorageEnabledMembers
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfStorageEnabledMembers)
                .setExtendProxyCount(numberOfExtendProxyMembers)
                .build();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeBeforeStop);
        assertThat(doesMemberExist(cluster, memberIdToStop), is(true));

        memberGroup.stopMember(memberIdToStop);

        sleepForSeconds(memberGroup.getSuggestedSleepAfterStopDuration());

        assertThatClusterIsExpectedSize(cluster, expectedClusterSizeAfterStop);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void attemptToStopMoreThanOneMemberWhichIsNotSupported() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(3)
                .build()
                .stopMember(1, 2);
    }
}
