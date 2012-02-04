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

package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.littlegrid.coherence.testsupport.impl.ReflectionDelegatingClusterMember;
import org.littlegrid.utils.ChildFirstUrlClassLoader;

import java.util.concurrent.TimeUnit;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;
import static org.littlegrid.coherence.testsupport.ClusterMemberGroupTestSupport.doesMemberExist;

/**
 * Reflection delegating cluster member fallback tests, to ensure fallback functionality
 * works as expected.
 */
public class ReflectionDelegatingClusterMemberFallbackIntegrationTest
        extends AbstractStorageDisabledClientClusterMemberGroupIntegrationTest {

    private static final int NUMBER_OF_MEMBERS = SMALL_TEST_CLUSTER_SIZE;
    private static final int EXPECTED_CLUSTER_SIZE = NUMBER_OF_MEMBERS + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
    private static final int MEMBER_ID = 2;

    private ClusterMemberGroup memberGroup;
    private ClusterMember member;


    @Before
    public void beforeTest() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(NUMBER_OF_MEMBERS)
                .setClusterMemberInstanceClassName(ReflectionDelegatingClusterMember.class.getName())
                .build();

        
        final Cluster cluster = CacheFactory.ensureCluster();

        assertThatClusterIsExpectedSize(cluster, EXPECTED_CLUSTER_SIZE);
        assertThat(doesMemberExist(cluster, MEMBER_ID), is(true));

        member = memberGroup.getClusterMember(MEMBER_ID);

        assertThat(member, notNullValue());
    }

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void fallbackShutdown() {
        member.shutdown();

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), EXPECTED_CLUSTER_SIZE - 1);
    }

    @Test
    public void fallbackStop()
            throws Exception {

        member.stop();

        TimeUnit.SECONDS.sleep(memberGroup.getSuggestedSleepAfterStopDuration());
        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), EXPECTED_CLUSTER_SIZE - 1);
    }

    @Test
    public void fallbackLocalMemberId() {
        assertThat(member.getLocalMemberId(), is(MEMBER_ID));
    }

    @Test
    public void fallbackContainingClassLoader() {
        assertThat(member.getActualContainingClassLoader(), instanceOf(ChildFirstUrlClassLoader.class));
    }

    /**
     * Delegate that doesn't bother to implement any methods that the reflection
     * delegating can call and thus the fallback method will always need to be called.
     */
    public static class NoMethodToCallDelegate {
    }
}
