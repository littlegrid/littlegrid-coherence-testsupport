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

package org.littlegrid.group.wka_autonomous;

import com.tangosol.net.CacheFactory;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;

/**
 * Cluster member group WKA tests.
 */
@Ignore
public final class WkaAndExtendPortIntegrationTest {
    private static final Logger LOGGER = Logger.getLogger(WkaAndExtendPortIntegrationTest.class.getName());

    @Test
    public void twoSmallMemberGroupsWithDifferentWkaAndExtendPorts() {
        final int offset = 100;
        final int numberOfMembers = SMALL_TEST_CLUSTER_SIZE;
        final int expectedClusterSize = numberOfMembers + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledExtendProxyCount(numberOfMembers)
                    .buildAndConfigureForNoClient();

            final int memberGroup2WkaPort = memberGroup1.getWkaPort() + offset;
            final int memberGroup2ExtendPort = memberGroup1.getExtendPort() + offset;

            LOGGER.warning(format(
                    "A different WKA port of %s has been configured for a WKA test, along with Extend port %s",
                    memberGroup2WkaPort, memberGroup2ExtendPort));

            memberGroup2 = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledExtendProxyCount(numberOfMembers)
                    .setWkaPort(memberGroup2WkaPort)
                    .setExtendPort(memberGroup2ExtendPort)
                    .buildAndConfigureForStorageDisabledClient();

            assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }
}
