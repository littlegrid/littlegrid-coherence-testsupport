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

import org.junit.Test;

import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Cluster member group WKA tests.
 */
public class ClusterMemberGroupWkaTest extends AbstractStorageDisabledClientClusterMemberGroupTest {
    @Test
    public void twoSmallMemberGroupsWithSameWka() {
        int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = (numberOfMembers * 2) + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfMembers).build();

            memberGroup2 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfMembers).build();
            assertThatClusterIsExpectedSize(expectedClusterSize);
        } finally {
            ClusterMemberGroupUtils.shutdownClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    @Test
    public void twoSmallMemberGroupsWithDifferentWkas() {
        int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            ClusterMemberGroup.Builder builder = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfMembers);
            memberGroup1 = builder.build();

            final int differentPort = builder.getWkaPort() + 20;
            LOGGER.warning(format("A different WKA port of '%s' has been configured for a WKA test", differentPort));

            memberGroup2 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setStorageEnabledCount(numberOfMembers)
                    .setWkaPort(differentPort).build();
            assertThatClusterIsExpectedSize(expectedClusterSize);
        } finally {
            ClusterMemberGroupUtils.shutdownClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }
}
