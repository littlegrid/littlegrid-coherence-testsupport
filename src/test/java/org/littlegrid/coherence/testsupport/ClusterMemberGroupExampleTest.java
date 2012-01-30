/*
 * Copyright (c) 2011, Jonathan Hall.
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

import com.tangosol.io.pof.PortableException;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;
import java.util.List;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

/**
 * Cluster member group example tests to show how to use the API.
 */
public class ClusterMemberGroupExampleTest extends AbstractClusterMemberGroupTest {
    private static final String KEY = "key";
    private static final String VALUE = "value";

    private ClusterMemberGroup memberGroup;

    @After
    public void afterTest() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void exampleOfSimplestUse() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder().build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfStorageEnabledMembersWithCacheConfiguration() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2).setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .build();

        performSimplePutSizeGet(KNOWN_TEST_CACHE);
    }

    @Test
    public void exampleOfOneStorageEnabledExtendProxyMember() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setStorageEnabledExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfOneExtendProxyAndNoStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE)
                .build();

        try {
            performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);

            fail("Exception was expected as there are no storage-enabled members");
        } catch (PortableException e) {
            // Coherence 3.6.x and above
        } catch (RuntimeException e) {
            // Coherence 3.5.x etc.
        }
    }

    @Test
    public void exampleOfExtendProxyWithSeparateStorageEnabledMembers() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .setExtendProxyCount(1)
                .setStorageEnabledCount(2)
                .setClientCacheConfiguration(EXTEND_CLIENT_CACHE_CONFIG_FILE).build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfConfiguringExtendProxyWithSeparateStorageEnabledMembersThroughProperties() {
        /*
            These properties could be read from a file.
         */
        Properties properties = new Properties();
        properties.setProperty("StorageEnabledCount", "2");
        properties.setProperty("ExtendProxyCount", "1");
        properties.setProperty("CacheConfiguration", TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE);
        properties.setProperty("ClientCacheConfiguration", EXTEND_CLIENT_CACHE_CONFIG_FILE);

        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setBuilderProperties(properties)
                .build();

        performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
    }

    @Test
    public void exampleOfTwoAutonomousClustersEachWithOneStorageEnabledExtendProxyMember()
            throws IOException {

        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;

        try {
            memberGroup1 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties("properties/memberGroup1.properties")
                    .build();

            memberGroup2 = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                    .setBuilderProperties("properties/memberGroup2.properties")
                    .build();

            performSimplePutSizeGet(KNOWN_EXTEND_TEST_CACHE);
        } finally {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup1, memberGroup2);
        }
    }

    @Test
    public void exampleOfAdditionalSystemProperties() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setAdditionalSystemProperties("properties/additionalSystemProperties.properties")
                .build();

        assertThat(System.getProperty("SystemPropertyThatShouldHaveBeenSet"), notNullValue());
    }

    @Test
    @Ignore
    public void exampleOfDifferentOverrideFileSpecified() {

    }
    
    @Test
    public void exampleOfUsingContainingClassLoaderToControlObject() 
            throws Exception {
        
        final int numberOfMembers = SINGLE_TEST_CLUSTER_SIZE;
        final int memberId = 1;
        
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setConfigurableMemberCount(numberOfMembers)
                .build();
        
        assertThat(memberGroup.getStartedMemberIds().length, is(numberOfMembers));

        final ClassLoader containingClassLoader =
                memberGroup.getClusterMember(memberId).getActualContainingClassLoader();

        System.out.println("ABOUT DO STUFF");
        final Class classWithinClusterMember = containingClassLoader.loadClass("java.util.Date");
        final Object object = classWithinClusterMember.newInstance();

        System.out.println("HERRRRRRRRRRRRRRRRRRR" + object.toString());
        System.out.println("DONESTUFF");
    }

    private void performSimplePutSizeGet(final String cacheName) {
        final NamedCache cache = CacheFactory.getCache(cacheName);
        cache.put(KEY, VALUE);

        assertThat(cache.size(), is(1));
        assertThat((String) cache.get(KEY), is(VALUE));
    }
}
