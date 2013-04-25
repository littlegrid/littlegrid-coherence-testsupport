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

package org.littlegrid.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;

/**
 * Usage counting cluster member group integration tests.
 */
public class UsageCountingClusterMemberGroupIntegrationTest {
    private static ClusterMemberGroup testClassMemberGroup;
    private static ClusterMemberGroup testMemberGroup;

    @BeforeClass
    public static void beforeTests() {
        testClassMemberGroup = ClusterMemberGroupUtils.newBuilder()
                .setClusterMemberGroupInstanceClassName(UsageCountingClusterMemberGroup.class.getName())
                .buildAndConfigureForStorageDisabledClient();
    }

    @AfterClass
    public static void afterTests() {
        final UsageCountingClusterMemberGroup instance = (UsageCountingClusterMemberGroup) testClassMemberGroup;

        assertThat(instance.getCount(), is(1));
        assertThat(instance.getMaximumCount(), is(2));
        assertThat(instance.getReuseCount(), is(4));

        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(testClassMemberGroup);
    }

    @Before
    public void beforeTest() {
        testMemberGroup = ClusterMemberGroupUtils.newBuilder()
                .setClusterMemberGroupInstanceClassName(UsageCountingClusterMemberGroup.class.getName())
                .buildAndConfigureForStorageDisabledClient();

//        CacheFactory.getCache(KNOWN_TEST_CACHE);
    }

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(testMemberGroup);
    }

    @Test
    public void test1() {
    }

    @Test
    public void test2() {
    }

    @Test
    public void test3() {
    }

    @Test
    public void test4() {
    }
}
