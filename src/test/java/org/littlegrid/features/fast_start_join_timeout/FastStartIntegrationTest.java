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

package org.littlegrid.features.fast_start_join_timeout;

import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.concurrent.TimeUnit;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Fast-start join timeout integration tests.
 */
public class FastStartIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void defaultDisabledSlowerThanIfTimeSpecified() {
        final long timeWithDefault;

        try {
            final long startTimeWithDefault = System.currentTimeMillis();

            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();

            timeWithDefault = System.currentTimeMillis() - startTimeWithDefault;
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }

        final long timeWithPurposelySlowJoinTimeout;

        try {
            final long startTime = System.currentTimeMillis();

            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(1)
                    .setFastStartJoinTimeoutMilliseconds(TimeUnit.SECONDS.toMillis(5))
                    .buildAndConfigureForStorageDisabledClient();

            timeWithPurposelySlowJoinTimeout = System.currentTimeMillis() - startTime;
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }

        assertThat(timeWithPurposelySlowJoinTimeout > timeWithDefault, is(true));
    }
}
