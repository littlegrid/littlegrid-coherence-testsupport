/*
 * Copyright (c) 2010-2013 Jonathan Hall.
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

package org.littlegrid;

import com.tangosol.net.CacheFactory;
import org.junit.Test;
import org.junit.Ignore;

import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;

/**
 * Cluster member group launcher integration tests.
 */
public class ClusterMemberGroupAppIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test(expected = UnsupportedOperationException.class)
    public void construct() {
        new ClusterMemberGroupApp();
    }

    @Test
    public void launchAndStartConsole() {
        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.launchAndStartConsole(
                new String[]{"properties/memberGroup1.properties"});

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), 3 + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP);

        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void start() {
        ClusterMemberGroupApp.main(new String[]{"properties/memberGroup1.properties"});
    }

    @Test
    @Ignore
    public void whatever() {
        System.setProperty("littlegrid.builder.AppConsoleClassName", "org.littlegrid.console.DefaultCommandConsole");
        System.setProperty("littlegrid.builder.StorageEnabledCount", "2");

        ClusterMemberGroupApp.main(new String[]{});
    }

    public static class NoWaitConsole {
        public static void main(String[] args) {
            System.out.println("Console launched - and done");
        }
    }
}
