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

import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Cluster member group utilities tests.
 */
public class ClusterMemberGroupUtilsTest {
    @Test
    public void attemptToShutdownNullMemberGroup() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups((ClusterMemberGroup) null);
    }

    @Test
    public void shutdownMemberGroupThatThrowsAnExceptionDuringItsShutdown() {
        final ClusterMemberGroup[] memberGroups = {
                new MockExceptionThrowingClusterMemberGroup(),
                new MockExceptionThrowingClusterMemberGroup()
        };

        try {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroups);
        } catch (IllegalStateException e) {
            // This is the expected exception
        }

        assertThat(MockExceptionThrowingClusterMemberGroup.getShutdownAllInvokedCounter(), is(memberGroups.length));
    }

    public static class MockExceptionThrowingClusterMemberGroup implements ClusterMemberGroup {
        private static int shutdownAllInvokedCounter;

        @Override
        public ClusterMemberGroup shutdownMember(final int... memberIds) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ClusterMemberGroup shutdownAll() {
            shutdownAllInvokedCounter++;

            throw new UnsupportedOperationException();
        }

        @Override
        public ClusterMemberGroup stopMember(final int... memberIds) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ClusterMemberGroup stopAll() {
            throw new UnsupportedOperationException();
        }

        @Override
        public ClusterMember getClusterMember(final int memberId) {
            throw new UnsupportedOperationException();
        }

        @Override
        public int[] getStartedMemberIds() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int getSuggestedSleepAfterStopDuration() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int merge(final ClusterMemberGroup memberGroup) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ClassLoader[] getActualContainingClassLoaders(final int... memberIds) {
            throw new UnsupportedOperationException();
        }

        public static int getShutdownAllInvokedCounter() {
            return shutdownAllInvokedCounter;
        }
    }
}
