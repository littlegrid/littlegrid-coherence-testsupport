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

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.support.SystemUtils;

import java.util.Map;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder;
import static org.littlegrid.impl.DefaultClusterMemberGroupBuilder.Registry;

/**
 * Default cluster member group builder tests.
 */
public final class DefaultClusterMemberGroupBuilderTest {
    private static final int EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE = 41;

    private static final String EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY = "ExceptionReporterInstanceClassName";
    private static final String CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY = "CallbackHandlerInstanceClassName";

    private static final String CUSTOM_CONFIGURED_MEMBER_COUNT_KEY = "CustomConfiguredCount";
    private static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    private static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME_KEY = "ClusterMemberGroupInstanceClassName";
    private static final String CUSTOM_CONFIGURED_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";

    private static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY = "SuggestedSleepAfterStopDurationDefault";

    private static final String JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY = "JarsToExcludeFromClassPath";
    private static final String CORE_JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY = "CoreJarsToExcludeFromClassPath";

    private static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";
    private static final String CLIENT_OVERRIDE_CONFIGURATION_KEY = "ClientOverrideConfiguration";
    private static final String CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY = "CustomConfiguredCacheConfiguration";

    private static final String CLUSTER_NAME_KEY = "ClusterName";
    private static final String SITE_NAME_KEY = "SiteName";
    private static final String RACK_NAME_KEY = "RackName";
    private static final String MACHINE_NAME_KEY = "MachineName";
    private static final String CUSTOM_CONFIGURED_MEMBER_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    private static final String STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    private static final String EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String JMX_MONITOR_ROLE_NAME_KEY = "JmxMonitorRoleName";
    private static final String STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
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

    private static final String FAST_START_JOIN_TIMEOUT_MILLISECONDS = "FastStartJoinTimeoutMilliseconds";

    private static final String BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY = "BuildAndConfigureForEnumName";
    private static final String APP_CONSOLE_CLASS_NAME_KEY = "AppConsoleClassName";

    private Properties systemPropertiesBeforeTest;


    @Before
    public void beforeTest() {
        systemPropertiesBeforeTest = SystemUtils.snapshotSystemProperties();

        // Ensure that override settings aren't picked up and applied - thus only the defaults are
        // used which these tests can then safely assert against.
        System.setProperty("littlegrid.builder.override", "non-existent-override-file.properties");
    }

    @After
    public void afterTest() {
        System.setProperties(systemPropertiesBeforeTest);
    }

    @Test
    @Ignore
    public void equalsWhenShouldBeEqual() {

    }

    @Test
    @Ignore
    public void equalsWhenShouldNotBeEquals() {

    }

    @Test
    public void nonCoherenceBuilderSettings() {
        final String expectedExceptionReportInstanceClassName = "com.g.h.i.BuildExceptionReporter";
        final String expectedCallbackHandlerInstanceClassName = "com.g.h.i.CallbackHandler";

        final int expectedCustomConfiguredMemberCount = 10;
        final int expectedStorageEnabledCount = 11;
        final int expectedStorageEnabledProxyCount = 12;
        final int expectedExtendProxyCount = 13;
        final int expectedJmxMonitorCount = 14;

        final int expectedNumberOfThreads = 18;
        final String expectedInstanceClassName = "com.a.b.c.ClusterMember";
        final String expectedGroupInstanceClassName = "com.a.b.c.ClusterMemberGroup";
        final String expectedCustomConfiguredInstanceClassName = "com.d.e.f.ClusterMember";

        final int expectedSleepDuration35x = 22;
        final int expectedSleepDuration36x = 23;
        final int expectedSleepDurationDefault = 24;

        final String jarsToExcludeFromClassPath = "abc.jar,def.jar";
        final String coreJarsToExcludeFromClassPath = "rt.jar";

        final String appConsoleClassName = "com.a.b.c.Console";
        final String buildAndConfigureForEnumName = STORAGE_DISABLED_CLIENT.name();

        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setExceptionReporterInstanceClassName(expectedExceptionReportInstanceClassName);
        builder.setCallbackHandlerInstanceClassName(expectedCallbackHandlerInstanceClassName);

        builder.setCustomConfiguredCount(expectedCustomConfiguredMemberCount);
        builder.setStorageEnabledCount(expectedStorageEnabledCount);
        builder.setStorageEnabledExtendProxyCount(expectedStorageEnabledProxyCount);
        builder.setExtendProxyCount(expectedExtendProxyCount);
        builder.setJmxMonitorCount(expectedJmxMonitorCount);

        builder.setNumberOfThreadsInStartUpPool(expectedNumberOfThreads);
        builder.setClusterMemberInstanceClassName(expectedInstanceClassName);
        builder.setClusterMemberGroupInstanceClassName(expectedGroupInstanceClassName);
        builder.setCustomConfiguredClusterMemberInstanceClassName(expectedCustomConfiguredInstanceClassName);

        builder.setSuggestedSleepAfterStopDuration35x(expectedSleepDuration35x);
        builder.setSuggestedSleepAfterStopDuration36x(expectedSleepDuration36x);
        builder.setSuggestedSleepAfterStopDurationDefault(expectedSleepDurationDefault);

        builder.setJarsToExcludeFromClassPath(jarsToExcludeFromClassPath);
        builder.setCoreJarsToExcludeFromClassPath(coreJarsToExcludeFromClassPath);

        builder.setAppConsoleClassName(appConsoleClassName);
        builder.setBuildAndConfigureForEnumName(buildAndConfigureForEnumName);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderKeysAndValues();

        assertThat(builderSettings.size(), is(EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE));

        assertThat(builderSettings.get(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY),
                is(expectedExceptionReportInstanceClassName));

        assertThat(builderSettings.get(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY),
                is(expectedCallbackHandlerInstanceClassName));

        assertThat(builderSettings.get(CUSTOM_CONFIGURED_MEMBER_COUNT_KEY),
                is(Integer.toString(expectedCustomConfiguredMemberCount)));

        assertThat(builderSettings.get(STORAGE_ENABLED_COUNT_KEY), is(Integer.toString(expectedStorageEnabledCount)));
        assertThat(builderSettings.get(STORAGE_ENABLED_PROXY_COUNT_KEY),
                is(Integer.toString(expectedStorageEnabledProxyCount)));

        assertThat(builderSettings.get(EXTEND_PROXY_COUNT_KEY), is(Integer.toString(expectedExtendProxyCount)));
        assertThat(builderSettings.get(JMX_MONITOR_COUNT_KEY), is(Integer.toString(expectedJmxMonitorCount)));

        assertThat(builderSettings.get(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY),
                is(Integer.toString(expectedNumberOfThreads)));

        assertThat(builderSettings.get(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY), is(expectedInstanceClassName));
        assertThat(builderSettings.get(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME_KEY),
                is(expectedGroupInstanceClassName));

        assertThat(builderSettings.get(CUSTOM_CONFIGURED_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY),
                is(expectedCustomConfiguredInstanceClassName));

        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_35X_KEY),
                is(Integer.toString(expectedSleepDuration35x)));

        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_36X_KEY),
                is(Integer.toString(expectedSleepDuration36x)));

        assertThat(builderSettings.get(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY),
                is(Integer.toString(expectedSleepDurationDefault)));

        assertThat(builderSettings.get(JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY), is(jarsToExcludeFromClassPath));
        assertThat(builderSettings.get(CORE_JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY), is(coreJarsToExcludeFromClassPath));

        assertThat(builderSettings.get(APP_CONSOLE_CLASS_NAME_KEY), is(appConsoleClassName));
        assertThat(builderSettings.get(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY), is(buildAndConfigureForEnumName));
    }

    @Test
    public void coherenceSystemPropertyBuilderSettings() {
        final String expectedCacheConfiguration = "cache-configuration.xml";
        final String expectedClientCacheConfiguration = "client-cache-configuration.xml";
        final String expectedOverrideConfiguration = "override-configuration.xml";
        final String expectedClientOverrideConfiguration = "client-override-configuration.xml";
        final String expectedCustomConfiguredCacheConfiguration = "custom-configured-cache-configuration.xml";

        final String expectedClusterName = "cluster-name";
        final String expectedSiteName = "site-name";
        final String expectedRackName = "rack-name";
        final String expectedMachineName = "machine-name";
        final String expectedCustomConfiguredRoleName = "custom-configurable-member";
        final String expectedStorageEnabledRoleName = "storage-enabled";
        final String expectedStorageEnabledProxyRoleName = "storage-enabled-proxy";
        final String expectedExtendProxyRoleName = "extend-proxy";
        final String expectedJmxRoleRoleName = "jmx-monitor";
        final String expectedStorageDisabledClientRoleName = "storage-disabled-client";
        final String expectedExtendClientRoleName = "extend-client";

        final String expectedWkaAddress = "234.234.234.0";
        final int expectedWkaPort = 12345;
        final int expectedExtendPort = 23456;
        final int expectedTtl = 3;

        final String expectedLogDestination = "log4j";
        final int expectedLogLevel = 8;

        final int expectedFastStartJoinTimeoutMilliseconds = 231;

        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setCacheConfiguration(expectedCacheConfiguration);
        builder.setClientCacheConfiguration(expectedClientCacheConfiguration);
        builder.setOverrideConfiguration(expectedOverrideConfiguration);
        builder.setClientOverrideConfiguration(expectedClientOverrideConfiguration);
        builder.setCustomConfiguredCacheConfiguration(expectedCustomConfiguredCacheConfiguration);

        builder.setClusterName(expectedClusterName);
        builder.setSiteName(expectedSiteName);
        builder.setRackName(expectedRackName);
        builder.setMachineName(expectedMachineName);
        builder.setCustomConfiguredRoleName(expectedCustomConfiguredRoleName);
        builder.setStorageEnabledRoleName(expectedStorageEnabledRoleName);
        builder.setStorageEnabledExtendProxyRoleName(expectedStorageEnabledProxyRoleName);
        builder.setExtendProxyRoleName(expectedExtendProxyRoleName);
        builder.setJmxMonitorRoleName(expectedJmxRoleRoleName);
        builder.setStorageDisabledClientRoleName(expectedStorageDisabledClientRoleName);
        builder.setExtendClientRoleName(expectedExtendClientRoleName);

        builder.setWkaAddress(expectedWkaAddress);
        builder.setWkaPort(expectedWkaPort);
        builder.setExtendPort(expectedExtendPort);
        builder.setTtl(expectedTtl);

        builder.setLogDestination(expectedLogDestination);
        builder.setLogLevel(expectedLogLevel);

        builder.setFastStartJoinTimeoutMilliseconds(expectedFastStartJoinTimeoutMilliseconds);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderKeysAndValues();

        assertThat(builderSettings.size(), is(EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE));


        assertThat(builderSettings.get(CACHE_CONFIGURATION_KEY), is(expectedCacheConfiguration));

        assertThat(builderSettings.get(CLIENT_CACHE_CONFIGURATION_KEY), is(expectedClientCacheConfiguration));
        assertThat(builder.getClientCacheConfiguration(), is(expectedClientCacheConfiguration));

        assertThat(builderSettings.get(OVERRIDE_CONFIGURATION_KEY), is(expectedOverrideConfiguration));
        assertThat(builderSettings.get(CLIENT_OVERRIDE_CONFIGURATION_KEY), is(expectedClientOverrideConfiguration));
        assertThat(builderSettings.get(CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY), is(expectedCustomConfiguredCacheConfiguration));

        assertThat(builderSettings.get(CLUSTER_NAME_KEY), is(expectedClusterName));
        assertThat(builderSettings.get(SITE_NAME_KEY), is(expectedSiteName));
        assertThat(builderSettings.get(RACK_NAME_KEY), is(expectedRackName));
        assertThat(builderSettings.get(MACHINE_NAME_KEY), is(expectedMachineName));
        assertThat(builderSettings.get(CUSTOM_CONFIGURED_MEMBER_ROLE_NAME_KEY), is(expectedCustomConfiguredRoleName));
        assertThat(builderSettings.get(STORAGE_ENABLED_ROLE_NAME_KEY), is(expectedStorageEnabledRoleName));
        assertThat(builderSettings.get(STORAGE_ENABLED_PROXY_ROLE_NAME_KEY), is(expectedStorageEnabledProxyRoleName));
        assertThat(builderSettings.get(EXTEND_PROXY_ROLE_NAME_KEY), is(expectedExtendProxyRoleName));
        assertThat(builderSettings.get(JMX_MONITOR_ROLE_NAME_KEY), is(expectedJmxRoleRoleName));
        assertThat(builderSettings.get(STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY),
                is(expectedStorageDisabledClientRoleName));

        assertThat(builderSettings.get(EXTEND_CLIENT_ROLE_NAME_KEY), is(expectedExtendClientRoleName));

        assertThat(builderSettings.get(WKA_ADDRESS_KEY), is(expectedWkaAddress));
        assertThat(builder.getWkaAddress(), is(expectedWkaAddress));

        assertThat(builderSettings.get(WKA_PORT_KEY), is(Integer.toString(expectedWkaPort)));
        assertThat(builder.getWkaPort(), is(expectedWkaPort));

        assertThat(builderSettings.get(EXTEND_PORT_KEY), is(Integer.toString(expectedExtendPort)));
        assertThat(builder.getExtendPort(), is(expectedExtendPort));

        assertThat(builderSettings.get(TTL_KEY), is(Integer.toString(expectedTtl)));

        assertThat(builderSettings.get(LOG_LEVEL_KEY), is(Integer.toString(expectedLogLevel)));
        assertThat(builderSettings.get(LOG_DESTINATION_KEY), is(expectedLogDestination));

        assertThat(builderSettings.get(FAST_START_JOIN_TIMEOUT_MILLISECONDS),
                is(Integer.toString(expectedFastStartJoinTimeoutMilliseconds)));

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
    public void setAndGetBuilderValueAsInt() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        final String key = "key";
        final int value = 123;

        defaultBuilder.setBuilderValue(key, value);

        assertThat(defaultBuilder.getBuilderValueAsInt(key), is(value));
    }

    @Test
    public void setAndGetBuilderValueAsLong() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        final String key = "key";
        final long value = 123L;

        defaultBuilder.setBuilderValue(key, value);

        assertThat(defaultBuilder.getBuilderValueAsLong(key), is(value));
    }

    @Test
    public void setAndGetBuilderValueAsString() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        final String key = "key";
        final String value = "123";

        defaultBuilder.setBuilderValue(key, value);

        assertThat(defaultBuilder.getBuilderValueAsString(key), is(value));
    }

    @Test
    public void getBuilderValueAsStringWhenNoEntry() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        assertThat(defaultBuilder.getBuilderValueAsString("no-entry"), nullValue());
    }

    @Test
    public void setAndGetBuilderValueAsStringWhenValueIsInt() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        final String key = "key";
        final String value = "123";

        defaultBuilder.setBuilderValue(key, Integer.parseInt(value));

        assertThat(defaultBuilder.getBuilderValueAsString(key), is(value));
    }

    private DefaultClusterMemberGroupBuilder getDefaultClusterMemberGroupBuilder() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        return (DefaultClusterMemberGroupBuilder) builder;
    }

    @Test
    public void performToString() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        assertThat(builder.toString().length() > 0, is(true));
    }

    @Test(expected = IllegalStateException.class)
    public void getPropertyNameFromMappingWhenDoesNotExist() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        defaultBuilder.getPropertyNameFromMapping("UnknownKey");
    }

    @Test(expected = IllegalArgumentException.class)
    public void setPropertyWhenValidWhenKeyIsNull() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();

        defaultBuilder.setPropertyWhenValid(new Properties(), null, null);
    }

    @Test
    public void setPropertyWhenValidWhenValueIsNull() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();
        final Properties properties = new Properties();

        defaultBuilder.setPropertyWhenValid(properties, CACHE_CONFIGURATION_KEY, null);

        assertThat(properties.size(), is(0));
    }

    @Test
    public void setPropertyWhenValidWhenValueIsSpaces() {
        final DefaultClusterMemberGroupBuilder defaultBuilder = getDefaultClusterMemberGroupBuilder();
        final Properties properties = new Properties();

        defaultBuilder.setPropertyWhenValid(properties, CACHE_CONFIGURATION_KEY, "   ");

        assertThat(properties.size(), is(0));
    }

    @Test
    public void defaultMappingSystemPropertiesForStorageEnabledWhenNoSpecificSettings() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageEnabled();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForStorageEnabledWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setStorageEnabledCacheConfiguration(expectedCacheConfiguration);

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageEnabled();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), is(expectedCacheConfiguration));
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyWhenNoSpecificSettings() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxy(123);

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setExtendProxyCacheConfiguration(expectedCacheConfiguration);

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxy(123);

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), is(expectedCacheConfiguration));
    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForStorageEnabledExtendProxy() {

    }

    @Test
    public void defaultMappingSystemPropertiesForStorageDisabledClientNoSpecificSettings() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageDisabledClient();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForStorageDisabledClientSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedOverrideConfiguration = "override-config.xml";

        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setClientCacheConfiguration(expectedCacheConfiguration);
        builder.setClientOverrideConfiguration(expectedOverrideConfiguration);

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageDisabledClient();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), is(expectedCacheConfiguration));
        assertThat(properties.getProperty("tangosol.coherence.override"), is(expectedOverrideConfiguration));
    }

    @Test
    public void defaultMappingSystemPropertiesForJmxMonitorClientWhenNoSpecificSettings() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForJmxMonitor();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForJmxMonitorClientWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setJmxMonitorCacheConfiguration(expectedCacheConfiguration);

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForJmxMonitor();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), is(expectedCacheConfiguration));
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyClientWhenNoSpecificSettings() {
        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxyClient();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyClientWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedOverrideConfiguration = "override-config.xml";

        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setClientCacheConfiguration(expectedCacheConfiguration);
        builder.setClientOverrideConfiguration(expectedOverrideConfiguration);

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxyClient();

        assertThat(properties.getProperty("tangosol.coherence.cacheconfig"), is(expectedCacheConfiguration));
        assertThat(properties.getProperty("tangosol.coherence.override"), is(expectedOverrideConfiguration));
    }

    @Test
    public void registryGetWhenEntryDoesNotExist() {
        final Registry registry = getRegistryAndClearContents();

        final Object key = ClusterMemberGroupUtils.newBuilder().toString();

        assertThat(registry.clusterMemberGroupMap.size(), is(0));
        assertThat(registry.getClusterMemberGroup(key), nullValue());
    }

    private Registry getRegistryAndClearContents() {
        final Registry registry = Registry.getInstance();
        registry.clusterMemberGroupMap.clear();
        return registry;
    }

    @Test
    public void registryGetWhenEntryDoesExist() {
        final Registry registry = getRegistryAndClearContents();

        final Object key = ClusterMemberGroupUtils.newBuilder().toString();

        registry.clusterMemberGroupMap.put(key, getClusterMemberGroup());

        assertThat(registry.clusterMemberGroupMap.size(), is(1));
        assertThat(registry.getClusterMemberGroup(key), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesNotExist() {
        final Registry registry = getRegistryAndClearContents();

        final Object key = ClusterMemberGroupUtils.newBuilder().toString();

        registry.registerClusterMemberGroup(key, getClusterMemberGroup());
        assertThat(registry.clusterMemberGroupMap.size(), is(1));
        assertThat(registry.getClusterMemberGroup(key), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesExist() {
        final Registry registry = getRegistryAndClearContents();

        final Object key = ClusterMemberGroupUtils.newBuilder().toString();

        {
            final ClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(key, memberGroup);
            assertThat(registry.clusterMemberGroupMap.size(), is(1));
            assertThat(registry.getClusterMemberGroup(key), notNullValue());
            assertThat(registry.getClusterMemberGroup(key), is(memberGroup));
        }

        {
            final ClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(key, memberGroup);
            assertThat(registry.clusterMemberGroupMap.size(), is(1));
            assertThat(registry.getClusterMemberGroup(key), notNullValue());
            assertThat(registry.getClusterMemberGroup(key), is(memberGroup));
        }
    }

    private DefaultClusterMemberGroup getClusterMemberGroup() {
        return new DefaultClusterMemberGroup(new DefaultCallbackHandler(), 0, 0, 0, 0, 0);
    }
}
