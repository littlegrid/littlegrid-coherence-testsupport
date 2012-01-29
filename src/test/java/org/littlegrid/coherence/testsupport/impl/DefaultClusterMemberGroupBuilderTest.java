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

package org.littlegrid.coherence.testsupport.impl;

import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.coherence.testsupport.ClusterMemberGroup;
import org.littlegrid.coherence.testsupport.ClusterMemberGroupUtils;

import java.util.Map;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Default cluster member group builder tests.
 */
public class DefaultClusterMemberGroupBuilderTest {
    private static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";

    private static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";

    private static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "version35xSleepAfterStopDuration";
    private static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "version36xSleepAfterStopDuration";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY = "defaultSleepAfterStopDuration";

    private static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";

    private static final String CLUSTER_NAME_KEY = "ClusterName";
    private static final String STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
    private static final String STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    private static final String EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";

    private static final String WKA_PORT_KEY = "WkaPort";
    private static final String WKA_ADDRESS_KEY = "WkaAddress";
    private static final String TTL_KEY = "Ttl";
    private static final String LOCAL_ADDRESS_KEY = "LocalAddress";
    private static final String LOCAL_PORT_KEY = "LocalPort";
    private static final String EXTEND_PORT_KEY = "ExtendPort";
    private static final String EXTEND_ADDRESS_KEY = "ExtendAddress";

    private static final String LOG_DESTINATION_KEY = "LogDestination";
    private static final String LOG_LEVEL_KEY = "LogLevel";

    private static final String DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    private static final String TCMP_ENABLED_KEY = "TcmpEnabled";
    private static final String EXTEND_ENABLED_KEY = "ExtendEnabled";


    @Test
    public void nonSystemPropertyBuilderSettings() {
//        setAdditionalSystemProperties
//        setJarsToExcludeFromClassPath
//        setBuilderProperties

        final int expectedStorageEnabledCount = 11;
        final int expectedExtendProxyCount = 12;
        final int expectedStorageEnabledProxyCount = 13;

        final int expectedNumberOfThreads = 14;
        final String expectedInstanceClassName = "com.a.b.c.ClusterMember";

        final int expectedSleepDuration35x = 22;
        final int expectedSleepDuration36x = 23;
        final int expectedSleepDurationDefault = 24;

        final ClusterMemberGroup.Builder builder = ClusterMemberGroupUtils.newClusterMemberGroupBuilder();

        builder.setStorageEnabledCount(expectedStorageEnabledCount);
        builder.setExtendProxyCount(expectedExtendProxyCount);
        builder.setStorageEnabledExtendProxyCount(expectedStorageEnabledProxyCount);

        builder.setNumberOfThreadsInStartUpPool(expectedNumberOfThreads);
        builder.setClusterMemberInstanceClassName(expectedInstanceClassName);

        builder.setSuggestedSleepAfterStopDuration35x(expectedSleepDuration35x);
        builder.setSuggestedSleepAfterStopDuration36x(expectedSleepDuration36x);
        builder.setSuggestedSleepAfterStopDurationDefault(expectedSleepDurationDefault);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderSettings();

        assertThat(builderSettings.get(STORAGE_ENABLED_COUNT_KEY), is(Integer.toString(expectedStorageEnabledCount)));
        assertThat(builderSettings.get(EXTEND_PROXY_COUNT_KEY), is(Integer.toString(expectedExtendProxyCount)));
        assertThat(builderSettings.get(STORAGE_ENABLED_PROXY_COUNT_KEY), is(Integer.toString(expectedStorageEnabledProxyCount)));

        assertThat(builderSettings.get(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY), is(Integer.toString(expectedNumberOfThreads)));
        assertThat(builderSettings.get(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY), is(expectedInstanceClassName));

        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_35X_KEY), is(Integer.toString(expectedSleepDuration35x)));
        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_36X_KEY), is(Integer.toString(expectedSleepDuration36x)));
        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY), is(Integer.toString(expectedSleepDurationDefault)));
    }

    @Test
    public void systemPropertyBuilderSettings() {
        /*
        setStorageEnabledSpecificCacheConfiguration
        setExtendProxySpecificCacheConfiguration
        setClientOverrideConfiguration
         */
        final String expectedCacheConfiguration = "cache-configuration.xml";
        final String expectedClientCacheConfiguration = "client-cache-configuration.xml";
        final String expectedOverrideConfiguration = "override-configuration.xml";

        final String expectedClusterName = "cluster-name";
        final String expectedStorageEnabledRoleName = "storage-enabled";
        final String expectedStorageDisabledClientRoleName = "storage-disabled";
        final String expectedExtendProxyRoleName = "extend-proxy";
        final String expectedStorageEnabledProxyRoleName = "storage-enabled-proxy";
        final String expectedExtendClientRoleName = "extend-client";

        final String expectedWkaAddress = "234.234.234.0";
        final int expectedWkaPort = 12345;
        final int expectedExtendPort = 23456;
        final int expectedTtl = 3;

        final String expectedLogDestination = "log4j";
        final int expectedLogLevel = 8;

        final ClusterMemberGroup.Builder builder = ClusterMemberGroupUtils.newClusterMemberGroupBuilder();

        builder.setCacheConfiguration(expectedCacheConfiguration);
        builder.setClientCacheConfiguration(expectedClientCacheConfiguration);
        builder.setOverrideConfiguration(expectedOverrideConfiguration);

        builder.setClusterName(expectedClusterName);
        builder.setStorageEnabledRoleName(expectedStorageEnabledRoleName);
        builder.setStorageDisabledClientRoleName(expectedStorageDisabledClientRoleName);
        builder.setExtendProxyRoleName(expectedExtendProxyRoleName);
        builder.setStorageEnabledExtendProxyRoleName(expectedStorageEnabledProxyRoleName);
        builder.setExtendClientRoleName(expectedExtendClientRoleName);

        builder.setWkaAddress(expectedWkaAddress);
        builder.setWkaPort(expectedWkaPort);
        builder.setExtendPort(expectedExtendPort);
        builder.setTtl(expectedTtl);

        builder.setLogDestination(expectedLogDestination);
        builder.setLogLevel(expectedLogLevel);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderSettings();

        assertThat(builderSettings.get(CACHE_CONFIGURATION_KEY), is(expectedCacheConfiguration));
        assertThat(builderSettings.get(CLIENT_CACHE_CONFIGURATION_KEY), is(expectedClientCacheConfiguration));
        assertThat(builderSettings.get(OVERRIDE_CONFIGURATION_KEY), is(expectedOverrideConfiguration));

        assertThat(builderSettings.get(CLUSTER_NAME_KEY), is(expectedClusterName));
        assertThat(builderSettings.get(STORAGE_ENABLED_ROLE_NAME_KEY), is(expectedStorageEnabledRoleName));
        assertThat(builderSettings.get(STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY), is(expectedStorageDisabledClientRoleName));
        assertThat(builderSettings.get(EXTEND_PROXY_ROLE_NAME_KEY), is(expectedExtendProxyRoleName));
        assertThat(builderSettings.get(STORAGE_ENABLED_PROXY_ROLE_NAME_KEY), is(expectedStorageEnabledProxyRoleName));
        assertThat(builderSettings.get(EXTEND_CLIENT_ROLE_NAME_KEY), is(expectedExtendClientRoleName));

        assertThat(builderSettings.get(WKA_ADDRESS_KEY), is(expectedWkaAddress));
        assertThat(builderSettings.get(WKA_PORT_KEY), is(Integer.toString(expectedWkaPort)));
        assertThat(builderSettings.get(EXTEND_PORT_KEY), is(Integer.toString(expectedExtendPort)));
        assertThat(builderSettings.get(TTL_KEY), is(Integer.toString(expectedTtl)));

        assertThat(builderSettings.get(LOG_LEVEL_KEY), is(Integer.toString(expectedLogLevel)));
        assertThat(builderSettings.get(LOG_DESTINATION_KEY), is(expectedLogDestination));

        // Values that are set at the point of the system properties being prepared using WKA values.
        assertThat(builderSettings.get(LOCAL_ADDRESS_KEY), nullValue());
        assertThat(builderSettings.get(LOCAL_PORT_KEY), nullValue());
        assertThat(builderSettings.get(EXTEND_ADDRESS_KEY), nullValue());

        // Values that are defaulted when the system properties are prepared, based upon the type of member.
        assertThat(builderSettings.get(DISTRIBUTED_LOCAL_STORAGE_KEY), nullValue());
        assertThat(builderSettings.get(TCMP_ENABLED_KEY), nullValue());
        assertThat(builderSettings.get(EXTEND_ENABLED_KEY), nullValue());
    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForStorageEnabled() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedOverrideConfiguration = "override-config.xml";

        final String expectedWkaAddress = "234.234.234.123";
        final String expectedLocalAddress = expectedWkaAddress;
        final int expectedWkaPort = 12345;
        final int expectedLocalPort = expectedWkaPort;
        final int expectedTtl = 2;

        final String expectedClusterName = "cluster-a";
        final String expectedStorageEnabledRoleName = "storage-enabled";

        final String expectedLogDestination = "stdout";
        final int expectedLogLevel = 3;

        final ClusterMemberGroup.Builder builder = ClusterMemberGroupUtils.newClusterMemberGroupBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;

        builder.setCacheConfiguration(expectedCacheConfiguration);
        builder.setOverrideConfiguration(expectedOverrideConfiguration);

        builder.setWkaAddress(expectedWkaAddress);
        builder.setWkaPort(expectedWkaPort);
        builder.setTtl(expectedTtl);

        builder.setClusterName(expectedClusterName);
        builder.setStorageEnabledRoleName(expectedStorageEnabledRoleName);

        builder.setLogDestination(expectedLogDestination);
        builder.setLogLevel(expectedLogLevel);

        final Properties properties = defaultBuilder.getSystemPropertiesForStorageEnabled();

        final boolean expectedTcmpEnabled = true;
        final boolean expectedDistributedLocalStorage = true;

        assertThat(properties.getProperty("tangosol.coherence.tcmp.enabled"), is(Boolean.toString(expectedTcmpEnabled)));
        assertThat(properties.getProperty("tangosol.coherence.distributed.localstorage"), is(Boolean.toString(expectedDistributedLocalStorage)));

        assertThat(properties.getProperty("tangosol.coherence.wka"), is(expectedWkaAddress));
        assertThat(properties.getProperty("tangosol.coherence.localhost"), is(expectedLocalAddress));
        assertThat(properties.getProperty("tangosol.coherence.wka.port"), is(Integer.toString(expectedWkaPort)));
        assertThat(properties.getProperty("tangosol.coherence.localport"), is(Integer.toString(expectedLocalPort)));

        assertThat(properties.getProperty("tangosol.coherence.cluster"), is(expectedClusterName));
        assertThat(properties.getProperty("tangosol.coherence.role"), is(expectedStorageEnabledRoleName));

        assertThat(properties.getProperty("tangosol.coherence.log"), is(expectedLogDestination));
        assertThat(properties.getProperty("tangosol.coherence.log.level"), is(Integer.toString(expectedLogLevel)));
    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForExtendProxy() {

    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForStorageEnabledExtendProxy() {

    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForStorageDisabledClient() {

    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForExtendProxyClient() {

    }
}
