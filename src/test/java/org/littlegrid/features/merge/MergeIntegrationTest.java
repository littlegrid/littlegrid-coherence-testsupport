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

package org.littlegrid.features.merge;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroup.Builder;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;
import static org.littlegrid.ClusterMemberGroupTestSupport.doesMemberExist;

/**
 * Merge cluster member group integration tests.
 */
@Ignore
public class MergeIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void incrementallyMergeInNewMemberGroups() {
        final int numberOfMembersToStartWith = 1;
        final int numberOfMembersToAddEachTime = 1;
        final int numberOfTimesToPerformMerge = 3;

        final Builder builder = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembersToStartWith);

        memberGroup = builder.buildAndConfigureForStorageDisabledClient();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThat(cluster.getMemberSet().size(), is(numberOfMembersToStartWith
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));

        builder.setStorageEnabledCount(numberOfMembersToAddEachTime);

        for (int i = 1; i <= numberOfTimesToPerformMerge; i++) {
            final int currentNumberOfMembers = memberGroup.merge(builder.buildAndConfigureForNoClient());

            final int expectedCurrentNumberOfMembers = numberOfMembersToStartWith + (numberOfMembersToAddEachTime * i);

            assertThat(currentNumberOfMembers, is(expectedCurrentNumberOfMembers));
            assertThatClusterIsExpectedSize(cluster, expectedCurrentNumberOfMembers
                    + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);
        }
    }

    @Test
    public void rollingRestart() {
        final int numberOfMembersToStartWith = 2;
        final int numberOfMembersToAddEachTime = 1;
        final int numberOfTimesToPerformMerge = 4;

        final Builder builder = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembersToStartWith);

        memberGroup = builder.buildAndConfigureForStorageDisabledClient();

        final Cluster cluster = CacheFactory.ensureCluster();

        assertThat(cluster.getMemberSet().size(), is(numberOfMembersToStartWith
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP));

        builder.setStorageEnabledCount(numberOfMembersToAddEachTime);

        for (int i = 1; i <= numberOfTimesToPerformMerge; i++) {
            // Roll new member(s) in
            memberGroup.merge(builder
                    .setFastStartJoinTimeoutMilliseconds(1000) // Ensure Coherence waits long enough before trying to create a new cluster
                    .buildAndConfigureForNoClient());

            assertThatClusterIsExpectedSize(cluster, numberOfMembersToStartWith + numberOfMembersToAddEachTime
                    + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

            final int idOfNextMemberToRollOutOfCluster = memberGroup.getStartedMemberIds()[0];

            // Roll the oldest member out
            memberGroup.shutdownMember(idOfNextMemberToRollOutOfCluster);

            assertThatClusterIsExpectedSize(cluster, numberOfMembersToStartWith
                    + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

            assertThat(doesMemberExist(cluster, idOfNextMemberToRollOutOfCluster), is(false));
        }
    }
}
