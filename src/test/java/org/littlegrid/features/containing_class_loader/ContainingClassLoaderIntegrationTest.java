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

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import com.tangosol.net.cache.AbstractCacheStore;
import com.tangosol.util.ClassHelper;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.features.PretendServer;
import org.littlegrid.support.ChildFirstUrlClassLoader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroup.ClusterMember;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.MEDIUM_TEST_CLUSTER_SIZE;
import static org.littlegrid.ClusterMemberGroupTestSupport.SMALL_TEST_CLUSTER_SIZE;

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
            final ClusterMember member = memberGroup.getClusterMember(memberId);

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
    @Ignore
    public void usingContainingClassLoaderToGetValue()
            throws Exception {

        final int expectedStoreCount = 10;
        final int writeDelay = 2;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(SMALL_TEST_CLUSTER_SIZE)
                .setCacheConfiguration("coherence/littlegrid-test-cache-store-cache-config.xml")
                .setAdditionalSystemProperty("example.cachestore", CountingCacheStore.class.getName())
                .setAdditionalSystemProperty("example.write.delay", writeDelay + "s")
                .setAdditionalSystemProperty("littlegrid.stub.cache.store.exception.keys", "3,4,5")
                .buildAndConfigureForStorageDisabledClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);

        for (int i = 0; i < expectedStoreCount; i++) {
            cache.put(i, i);
        }

        TimeUnit.SECONDS.sleep(writeDelay + 1); // wait an extra second

        int totalCount = 0;

        for (final ClassLoader classLoader : memberGroup.getActualContainingClassLoaders(
                memberGroup.getStartedMemberIds())) {

            final Class cacheStore = classLoader.loadClass(CountingCacheStore.class.getName());
            final int countForMember = (Integer) ClassHelper.invokeStatic(cacheStore, "getStoreCounter", new Object[]{});

            totalCount += countForMember;
        }

        assertThat(totalCount, is(expectedStoreCount));
    }

    @Test
    public void containingClassLoaderWhenNonStartedMemberIdsSpecified() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(SMALL_TEST_CLUSTER_SIZE)
                .buildAndConfigureForStorageDisabledClient();

        final ClassLoader[] classLoaders = memberGroup.getActualContainingClassLoaders(1, 3, 5, 7);

        assertThat(classLoaders.length, is(1));
    }


    public static class CountingCacheStore extends AbstractCacheStore {
        private static final Logger LOGGER = Logger.getLogger(CountingCacheStore.class.getName());

        private static final AtomicInteger loadCounter = new AtomicInteger();
        private static final AtomicInteger storeCounter = new AtomicInteger();

        private List<String> loadKeysThatWillGenerateExceptions = new ArrayList<String>();
        private List<String> storeKeysThatWillGenerateExceptions = new ArrayList<String>();

        public CountingCacheStore() {
            final String loadKeys = System.getProperty("littlegrid.stub.cache.load.exception.keys", "");
            loadKeysThatWillGenerateExceptions.addAll(Arrays.asList(loadKeys.split(",")));

            final String storeKeys = System.getProperty("littlegrid.stub.cache.store.exception.keys", "");
            storeKeysThatWillGenerateExceptions.addAll(Arrays.asList(storeKeys.split(",")));

            if (storeKeysThatWillGenerateExceptions.size() > 0) {
                LOGGER.info(format("The following keys will cause exceptions when store is invoked: %s",
                        storeKeysThatWillGenerateExceptions));
            }
        }

        @Override
        public Object load(final Object key) {
            if (loadKeysThatWillGenerateExceptions.contains(key.toString())) {
                throw new UnsupportedOperationException();
            }

            return loadCounter.incrementAndGet();
        }

        @Override
        public void store(final Object key,
                          final Object value) {

            if (storeKeysThatWillGenerateExceptions.contains(key.toString())) {
                throw new UnsupportedOperationException();
            }

            System.out.println("store");
            storeCounter.incrementAndGet();
        }

        public static int getLoadCounter() {
            return loadCounter.get();
        }

        public static int getStoreCounter() {
            return storeCounter.get();
        }
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
