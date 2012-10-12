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

package org.littlegrid.features.containing_class_loader;

import com.sun.org.apache.bcel.internal.generic.INSTANCEOF;
import com.tangosol.io.pof.PofReader;
import com.tangosol.io.pof.PofWriter;
import com.tangosol.io.pof.PortableObject;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import com.tangosol.util.ClassHelper;
import com.tangosol.util.InvocableMap;
import com.tangosol.util.processor.AbstractProcessor;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.features.PretendServer;
import org.littlegrid.support.ChildFirstUrlClassLoader;

import java.io.IOException;
import java.util.Date;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;

/**
 * Cluster member actual containing class loader tests.
 */
public final class ContainingClassLoaderIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void getContainingClassLoader() {
        final int numberOfMembers = 2;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .buildAndConfigureForStorageDisabledClient();

        final int[] memberIds = memberGroup.getStartedMemberIds();

        assertThat(memberIds.length, is(numberOfMembers));

        for (final int memberId : memberIds) {
            // This cluster member will be a wrapper around another class which is held in a
            // child-first class loader.
            final ClusterMemberGroup.ClusterMember member = memberGroup.getClusterMember(memberId);

            assertThat(member.getActualContainingClassLoader(), instanceOf(ChildFirstUrlClassLoader.class));

            // Check the the class loader that is containing the actual wrapped cluster member isn't the same
            // as the one that is wrapping the wrapped cluster member.
            assertThat(member.getActualContainingClassLoader(), not(member.getClass().getClassLoader()));
        }
    }

    @Test
    public void usingContainingClassLoaderToControlObject()
            throws Exception {

        final int numberOfMembers = MEDIUM_TEST_CLUSTER_SIZE;
        final int memberIdToRunPretendServerIn = 2;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .buildAndConfigureForStorageDisabledClient();

        assertThat(memberGroup.getStartedMemberIds().length, is(numberOfMembers));

        final ClassLoader containingClassLoader =
                memberGroup.getClusterMember(memberIdToRunPretendServerIn).getActualContainingClassLoader();

        final Class classWithinClusterMember = containingClassLoader.loadClass(PretendServer.class.getName());

        final Object pretendServer = classWithinClusterMember.newInstance();
        ClassHelper.invoke(pretendServer, "start", new Object[]{});
        ClassHelper.invoke(pretendServer, "shutdown", new Object[]{});
    }

/*
    @Test
    public void usingContainingClassLoaderToControlSingleton() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1)
                .buildAndConfigureForStorageDisabledClient();

        final ClassLoader containingClassLoader =
                memberGroup.getClusterMember(1).getActualContainingClassLoader();

        final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);
        final Date businessDate = (Date) cache.invoke("123", new BusinessDayProcessor());


    }
*/

    @Test
    public void usingContainingClassLoaderToGetValue() {

    }

/*
    public static class BusinessDayProcessor extends AbstractProcessor implements PortableObject {
        @Override
        public Object process(final InvocableMap.Entry entry) {
            return new Date();
//            throw new UnsupportedOperationException();
        }

        @Override
        public void readExternal(final PofReader reader)
                throws IOException {
        }

        @Override
        public void writeExternal(final PofWriter writer)
                throws IOException {
        }
    }

    public static class BusinessDay {
        private int day;
        private int month;
        private int year;

        private static BusinessDay INSTANCE;

        public static synchronized BusinessDay getInstance() {
            if (INSTANCE == null) {
                INSTANCE = new BusinessDay();
            }

            return INSTANCE;
        }

        public int getDay() {
            return day;
        }

        public int getMonth() {
            return month;
        }

        public int getYear() {
            return year;
        }
    }
*/
}
