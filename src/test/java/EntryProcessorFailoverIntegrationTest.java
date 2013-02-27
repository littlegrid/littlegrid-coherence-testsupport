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

import com.tangosol.net.CacheFactory;
import com.tangosol.net.DistributedCacheService;
import com.tangosol.net.NamedCache;
import com.tangosol.util.InvocableMap;
import com.tangosol.util.processor.AbstractProcessor;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertTrue;

/**
 * Entry processor failover integration tests - these tests demonstrate the different
 * behaviour when performing a stop or shutdown on a cluster member whilst it is executing
 * an entry processor, the tests also demonstrate how to use the standard Java Executor
 * framework to essentially perform 'deferred' events.
 */
public class EntryProcessorFailoverIntegrationTest {
    private static final int SECONDS_DELAY_BEFORE_DEFERRED_ACTION = 2;
    private static final int SECONDS_TO_SLEEP_IN_ENTRY_PROCESSOR = SECONDS_DELAY_BEFORE_DEFERRED_ACTION + 3;

    private ClusterMemberGroup memberGroup;
    private NamedCache cache;


    @Before
    public void beforeTest() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(3)
                .setCacheConfiguration("simple-cache-config.xml")
                .setLogLevel(3)
                .setFastStartJoinTimeoutMilliseconds(1000) // Coherence 3.7.x can use 100, older versions require 1000
                .buildAndConfigureForStorageDisabledClient();

        cache = CacheFactory.getCache("test");
    }

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void deferredStopAndEntryProcessorFailover() {
        final String key = "abc";
        final int idOfMemberToStop = ((DistributedCacheService) cache.getCacheService()).getKeyOwner(key).getId();

        final Runnable runnable = new Runnable() {
            @Override
            public void run() {
                // With a stop, the Coherence member leaves the cluster immediately without
                // telling the other members - hence it is like it has been killed
                memberGroup.stopMember(idOfMemberToStop);
            }
        };

        final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);
        executorService.schedule(runnable, SECONDS_DELAY_BEFORE_DEFERRED_ACTION, TimeUnit.SECONDS);

        final Integer idOfMemberThatCompletedProcessing = (Integer) cache.invoke(key, new SleepyProcessor());

        // The member being stopped whilst executing the entry processor is *not* expected
        // to be the member that originally started the processing - i.e. the processing should
        // be completed on the member who originally had the backup
        assertTrue(idOfMemberThatCompletedProcessing != idOfMemberToStop);
    }

    @Test
    public void deferredShutdownAndEntryProcessorFailover() throws InterruptedException {
        final String key = "abc";
        final int idOfMemberToShutdown = ((DistributedCacheService) cache.getCacheService()).getKeyOwner(key).getId();

        final Runnable runnable = new Runnable() {
            @Override
            public void run() {
                // With a shutdown, Coherence should let the entry processor finish on the original
                // member before the member leaves the cluster
                memberGroup.shutdownMember(idOfMemberToShutdown);
            }
        };

        final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);
        executorService.schedule(runnable, SECONDS_DELAY_BEFORE_DEFERRED_ACTION, TimeUnit.SECONDS);

        final Integer idOfMemberThatCompletedProcessing = (Integer) cache.invoke(key, new SleepyProcessor());

        // The member being shutdown whilst executing the entry processor is expected
        // to be the member that originally started the processing
        assertTrue(idOfMemberThatCompletedProcessing == idOfMemberToShutdown);
    }

    /**
     * Simple entry processor that sleeps enabling it to be failed over in
     * certain circumstances.
     */
    public static class SleepyProcessor extends AbstractProcessor {
        @Override
        public Object process(final InvocableMap.Entry entry) {
            final int memberId = CacheFactory.ensureCluster().getLocalMember().getId();

            CacheFactory.log("============ About to sleep");

            try {
                TimeUnit.SECONDS.sleep(SECONDS_TO_SLEEP_IN_ENTRY_PROCESSOR);
            } catch (InterruptedException e) {
                throw new IllegalStateException(e);
            }

            CacheFactory.log("============ Finished sleeping - survived!  ID of member that completed processing: "
                    + memberId);

            return memberId;
        }
    }
}
