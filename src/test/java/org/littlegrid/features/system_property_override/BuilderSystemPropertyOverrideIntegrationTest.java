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

package org.littlegrid.features.system_property_override;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupTestSupport;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_OVERRIDE_KEY;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_EXTEND_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;

/**
 * Builder system property override tests that use the littlegrid.builder.override system
 * property to specify an alternative properties file through a system property.
 */
public class BuilderSystemPropertyOverrideIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void alternativeOverrideFileSpecified() {
        System.setProperty(BUILDER_OVERRIDE_KEY,
                "directory-where-the-config-is-stored/example-littlegrid-builder-override.properties");

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .buildAndConfigureForStorageDisabledClient();

        // This example configuration file has a default storage-enabled member count of 3, so a cluster of
        // 4 in total with this storage-disabled member.
        assertThat(CacheFactory.ensureCluster().getMemberSet().size(), is(4));
    }

    @Test
    public void alternativeSystemPropertyMappingOverrideFileSpecified() {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY,
                "directory-where-the-config-is-stored/example-littlegrid-builder-system-property-mapping-override.properties");

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCacheConfiguration("coherence/littlegrid-test-cache-config-with-different-system-property-names.xml")
                .setClientCacheConfiguration("coherence/littlegrid-test-extend-client-cache-config-with-different-system-property-names.xml")
                .setStorageEnabledExtendProxyCount(1)
                .buildAndConfigureForExtendClient();

        final NamedCache cache = CacheFactory.getCache(KNOWN_EXTEND_TEST_CACHE);
        cache.put("key", "value");
    }
}
