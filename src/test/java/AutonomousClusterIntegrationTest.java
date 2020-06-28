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

import com.tangosol.net.CacheFactory;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.junit.Assert.assertEquals;

public class AutonomousClusterIntegrationTest {
    @Test
    public void twoAutonomousClusters() {
        final int numberOfMembers = 2;
        final int portOffset = 100;
        final int expectedClusterSize = numberOfMembers + 1; // Include this test which will join as a member

        ClusterMemberGroup memberGroupCluster1 = null;
        ClusterMemberGroup memberGroupCluster2 = null;

        try {
            // Build the first cluster (we won't actually be connecting to this one)
            memberGroupCluster1 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(numberOfMembers)
                    .buildAndConfigureForNoClient();

            // The second cluster will need to run on a different port to avoid clustering with
            // the first cluster
            final int member2WkaPort = memberGroupCluster1.getWkaPort() + portOffset;

            // Build the second cluster - we will join this cluster through this test by
            // asserting the cluster size
            memberGroupCluster2 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(numberOfMembers)
                    .setWkaPort(member2WkaPort)
                    .buildAndConfigureForStorageDisabledClient();

            // Check the size of the second cluster
            assertEquals(expectedClusterSize, CacheFactory.ensureCluster().getMemberSet().size());
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(
                    memberGroupCluster1, memberGroupCluster2);
        }
    }
}
