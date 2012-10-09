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

package org.littlegrid.features.setting_builder_properties;

import com.tangosol.net.CacheFactory;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.Properties;

import static org.littlegrid.ClusterMemberGroupTestSupport.CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;
import static org.littlegrid.ClusterMemberGroupTestSupport.EXTEND_CLIENT_CACHE_CONFIGURATION_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE;
import static org.littlegrid.ClusterMemberGroupTestSupport.assertThatClusterIsExpectedSize;

/**
 * Setting builder properties integration tests.
 */
public class SettingBuilderPropertiesIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void memberGroupConfiguredThroughPropertiesObject() {
        final int storageEnabledCount = 2;
        final int extendProxyCount = 1;
        final int expectedClusterSize = storageEnabledCount + extendProxyCount
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        Properties properties = new Properties();
        properties.setProperty("StorageEnabledCount", Integer.toString(storageEnabledCount));
        properties.setProperty("ExtendProxyCount", Integer.toString(extendProxyCount));
        properties.setProperty("CacheConfiguration", TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE);
        properties.setProperty("ClientCacheConfiguration", EXTEND_CLIENT_CACHE_CONFIGURATION_FILE);

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setBuilderProperties(properties)
                .buildAndConfigureForStorageDisabledClient();

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);
    }

    @Test
    public void memberGroupConfiguredThroughStringPropertiesFilename() {
        final int expectedClusterSize = 3 + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setBuilderProperties("properties/memberGroup1.properties")
                .buildAndConfigureForStorageDisabledClient();


        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);
    }

    @Test
    public void memberGroupConfiguredThroughMultipleStringPropertiesFilenames() {
        final int expectedClusterSize = 2
                + CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setBuilderProperties("properties/memberGroup1.properties", "properties/memberGroup2.properties")
                .buildAndConfigureForStorageDisabledClient();

        assertThatClusterIsExpectedSize(CacheFactory.ensureCluster(), expectedClusterSize);
    }
}
