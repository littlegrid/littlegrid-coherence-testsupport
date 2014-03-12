/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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

package org.littlegrid.features.group_callhandler;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import com.tangosol.util.extractor.ReflectionExtractor;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.impl.DefaultCallbackHandler;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroup.CallbackHandler;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;

/**
 * Cluster member group callback handler test.
 */
public class GroupCallbackHandlerTest extends AbstractAfterTestShutdownIntegrationTest {
    private static final int EXPECTED_COUNTED_TOTAL = 16;

    @Test
    public void buildForStorageDisabledClientCallbackHandlerAllCalled() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCallbackHandlerInstanceClassName(NumberAddCallbackHandler.class.getName())
                .buildAndConfigureForStorageDisabledClient();

        memberGroup.shutdownAll();

        assertThat(NumberAddCallbackHandler.getCounter(), is(EXPECTED_COUNTED_TOTAL));
    }

    @Test
    public void buildForExtendClientCallbackHandlerAllCalled() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCallbackHandlerInstanceClassName(NumberAddCallbackHandler.class.getName())
                .buildAndConfigureForExtendClient();

        memberGroup.shutdownAll();

        assertThat(NumberAddCallbackHandler.getCounter(), is(EXPECTED_COUNTED_TOTAL));
    }

    @Test
    public void buildForStorageEnabledMemberCallbackHandlerAllCalled() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCallbackHandlerInstanceClassName(NumberAddCallbackHandler.class.getName())
                .buildAndConfigureForStorageEnabledMember();

        memberGroup.shutdownAll();

        assertThat(NumberAddCallbackHandler.getCounter(), is(EXPECTED_COUNTED_TOTAL));
    }

    @Test
    public void buildForNoClientCallbackHandlerAllCalled() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCallbackHandlerInstanceClassName(NumberAddCallbackHandler.class.getName())
                .buildAndConfigureForNoClient();

        memberGroup.shutdownAll();

        assertThat(NumberAddCallbackHandler.getCounter(), is(EXPECTED_COUNTED_TOTAL));
    }

    @Test
    public void callbackHandlerAddIndexes() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1)
                .setCallbackHandlerInstanceClassName(PopulateCacheAndAddIndexesCallbackHandler.class.getName())
                .buildAndConfigureForStorageDisabledClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);

        assertThat(cache.size(), is(1));

        memberGroup.shutdownAll();
    }

    public static class PopulateCacheAndAddIndexesCallbackHandler extends DefaultCallbackHandler {
        @Override
        public void doAfterStart() {
            // Ensure that this member is storage disabled and thus should be joining the cluster already
            // established with a storage-enabled member.
            System.setProperty("tangosol.coherence.distributed.localstorage", "false");

            final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);

            cache.put("key", "value");

            cache.addIndex(new ReflectionExtractor("toString"), false, null);
        }
    }

    public static class NumberAddCallbackHandler implements CallbackHandler {
        private static int counter;

        public NumberAddCallbackHandler() {
            // Reset each time created - especially important as the counter is static to
            // enable it to be 'got at' for testing.
            counter = 0;
        }

        @Override
        public void doBeforeStart() {
            counter += 1;
        }

        @Override
        public void doAfterStart() {
            counter += 3;
        }

        @Override
        public void doBeforeShutdown() {
            counter += 5;
        }

        @Override
        public void doAfterShutdown() {
            counter += 7;
        }

        public static int getCounter() {
            return counter;
        }
    }
}
