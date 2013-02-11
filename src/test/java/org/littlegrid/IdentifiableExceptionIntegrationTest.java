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

import org.junit.Ignore;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.littlegrid.IdentifiableException.ReasonEnum.CHECK_CHILD_FIRST_CLASS_PATH_IN_USE;
import static org.littlegrid.IdentifiableException.ReasonEnum.JOIN_TIMEOUT_MILLISECONDS_TOO_SMALL;
import static org.littlegrid.IdentifiableException.ReasonEnum.SUSPECTED_AUTOSTART_EXCEPTION;

/**
 * Identifiable exception integration tests.
 */
public class IdentifiableExceptionIntegrationTest
        extends AbstractAfterTestShutdownIntegrationTest {

    @Test
    public void exceptionDueToCoherenceJarExcluded() {
        try {
            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(2)
                    .setJarsToExcludeFromClassPath("coherence")
                    .buildAndConfigureForNoClient();
        } catch (ClusterMemberGroupBuildException e) {
            final IdentifiableException identifiableException = (IdentifiableException) e.getCause();

            assertThat(identifiableException.getReasonEnum(), is(CHECK_CHILD_FIRST_CLASS_PATH_IN_USE));
        }
    }

    @Test
    public void exceptionDueToNoAutoStartedServicesThisTestIsForCoherence371AndAbove() {
        try {
            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(1)
                    .setCacheConfiguration("coherence/littlegrid-test-cache-config-with-no-autostart.xml")
                    .buildAndConfigureForStorageDisabledClient();

            fail("Note: this test is for Coherence 3.7.1.x and above - it will fail with older versions");
        } catch (ClusterMemberGroupBuildException e) {
            final IdentifiableException identifiableException = (IdentifiableException) e.getCause();

            assertThat(identifiableException.getReasonEnum(), is(SUSPECTED_AUTOSTART_EXCEPTION));
        }
    }

    @Test
    @Ignore
    public void exceptionDueToJoinTimeoutMillisTooSmall() {
        try {
            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(1)
                    .setFastStartJoinTimeoutMilliseconds(1)
                    .buildAndConfigureForStorageDisabledClient();

            fail("Note: this test is for Coherence 3.7.1.x and above - it will fail with older versions");
        } catch (ClusterMemberGroupBuildException e) {
            final IdentifiableException identifiableException = (IdentifiableException) e.getCause();

            assertThat(identifiableException.getReasonEnum(), is(JOIN_TIMEOUT_MILLISECONDS_TOO_SMALL));
        }
    }
}
