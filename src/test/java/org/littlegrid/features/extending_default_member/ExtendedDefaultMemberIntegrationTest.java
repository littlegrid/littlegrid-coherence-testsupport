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

package org.littlegrid.features.extending_default_member;

import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.features.PretendServer;
import org.littlegrid.impl.DefaultClusterMember;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.SINGLE_TEST_CLUSTER_SIZE;

/**
 * Extended default member integration tests.
 */
public class ExtendedDefaultMemberIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void exampleOfExtendingDefaultClusterMemberToUseLifeCycleMethods()
            throws Exception {

        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCustomConfiguredCount(numberOfMembers)
                .setCustomConfiguredClusterMemberInstanceClassName(PretendServerClusterMember.class.getName())
                .build();

        assertThat(memberGroup.getStartedMemberIds().length, is(numberOfMembers));
    }

    public static class PretendServerClusterMember extends DefaultClusterMember {
        private PretendServer server = new PretendServer();

        @Override
        public void doBeforeStart() {
            /*
                 At this point, Coherence hasn't been started in the other class loader - so functions
                 such as getting the member Id won't work (because it isn't running).

                 However, if you wanted to start out a JMS consumer or run a server then that is fine
                 because it will be in a different class loader.
             */

            System.out.println("Performing do before start - class loader: " + this.getClass().getClassLoader());
        }

        @Override
        public void doAfterStart() {
            server.start();
        }

        @Override
        public void doBeforeShutdown() {
            server.shutdown();
        }

        @Override
        public void doAfterShutdown() {
            /*
                 At this point, Coherence has been stopped - so functions such as getting the member Id
                 won't work (because it isn't running).

                 However, if you wanted to shutdown JMS consumer or shutdown a server then that is fine
                 because it will be in a different class loader.
             */

            System.out.println("Performing do after shutdown - class loader: " + this.getClass().getClassLoader());
        }
    }
}
