/*
 * Copyright (c) 2010-2020 Jonathan Hall.
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
import org.littlegrid.ClusterMemberGroupBuilder;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.ReusableClusterMemberGroup;
import org.littlegrid.support.SystemUtils;

import java.util.Map;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertThat;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroupBuilder.BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY;
import static org.littlegrid.impl.DefaultClusterMemberGroupBuilder.Registry;

/**
 * Default cluster member group builder tests.
 */
public final class DefaultClusterMemberGroupBuilderTest {
    private static final int EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE = 48;

    private static final String EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY = "ExceptionReporterInstanceClassName";
    private static final String CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY = "CallbackHandlerInstanceClassName";

    private static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    private static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME_KEY = "ClusterMemberGroupInstanceClassName";

    private static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY = "SuggestedSleepAfterStopDurationDefault";

    private static final String JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY = "JarsToExcludeFromClassPath";
    private static final String CORE_JARS_TO_EXCLUE_FROM_CLASS_PATH_KEY = "CoreJarsToExcludeFromClassPath";

    private static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";

    private static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String CLIENT_OVERRIDE_CONFIGURATION_KEY = "ClientOverrideConfiguration";
    private static final String CLIENT_LOG_LEVEL_KEY = "ClientLogLevel";
    private static final String STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
    private static final String EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";

    private static final String CUSTOM_CONFIGURED_MEMBER_COUNT_KEY = "CustomConfiguredCount";
    private static final String CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY = "CustomConfiguredCacheConfiguration";
    private static final String CUSTOM_CONFIGURED_MEMBER_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    private static final String CUSTOM_CONFIGURED_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";

    private static final String CLUSTER_NAME_KEY = "ClusterName";
    private static final String SITE_NAME_KEY = "SiteName";
    private static final String RACK_NAME_KEY = "RackName";
    private static final String MACHINE_NAME_KEY = "MachineName";

    private static final String STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String STORAGE_ENABLED_CACHE_CONFIGURATION_KEY = "StorageEnabledCacheConfiguration";
    private static final String STORAGE_ENABLED_LOG_LEVEL_KEY = "StorageEnabledLogLevel";

    private static final String STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";

    private static final String EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String EXTEND_PROXY_CACHE_CONFIGURATION_KEY = "ExtendProxyCacheConfiguration";
    private static final String EXTEND_PROXY_LOG_LEVEL_KEY = "ExtendProxyLogLevel";

    private static final String JMX_MONITOR_ROLE_NAME_KEY = "JmxMonitorRoleName";
    private static final String JMX_MONITOR_CACHE_CONFIGURATION_KEY = "JmxMonitorCacheConfiguration";
    private static final String JMX_MONITOR_LOG_LEVEL_KEY = "JmxMonitorLogLevel";

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

    private static final String COHERENCE_CACHE_CONFIG_KEY = "tangosol.coherence.cacheconfig";
    private static final String COHERENCE_LOG_LEVEL_KEY = "tangosol.coherence.log.level";
    private static final String COHERENCE_OVERRIDE_KEY = "tangosol.coherence.override";

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
    public void equalsWhenOtherIsThis() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        assertThat(builder.equals(builder), is(true));
    }

    @Test
    public void equalsWhenOtherIsNull() {
        assertThat(ClusterMemberGroupUtils.newBuilder().equals(null), is(false));
    }

    @Test
    public void equalsWhenOtherIsDifferentClass() {
        assertThat(ClusterMemberGroupUtils.newBuilder().equals("a-string"), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentAdditionalSystemProperties() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder()
                .setAdditionalSystemProperty("hasAdditionalSystemPropertyToBeDifferent", true);

        assertThat(thisBuilder.equals(otherBuilder), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentBuilderToSystemPropertyNameMappings() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();

        System.setProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY,
                "directory-where-config-stored/example-littlegrid-builder-system-property-mapping-override.properties");

        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder();

        assertThat(thisBuilder.equals(otherBuilder), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentBuilderKeyAndValues() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder()
                .setWkaPort(1234);

        assertThat(thisBuilder.equals(otherBuilder), is(false));
    }

    @Test
    public void equalsWhenOtherIsSame() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder();

        assertThat(thisBuilder.equals(otherBuilder), is(true));
    }

    @Test
    public void hashCodeWhenOtherIsDifferent() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder()
                .setWkaPort(1234);

        assertThat(otherBuilder.hashCode(), not(thisBuilder.hashCode()));
    }

    @Test
    public void hashCodeWhenOtherIsSame() {
        final ClusterMemberGroupBuilder thisBuilder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroupBuilder otherBuilder = ClusterMemberGroupUtils.newBuilder();

        assertThat(otherBuilder.hashCode(), is(thisBuilder.hashCode()));
    }

    @Test(expected = IllegalStateException.class)
    public void unknownMemberGroupInstanceClassName() {
        ClusterMemberGroupUtils.newBuilder()
                .setClusterMemberGroupInstanceClassName("com.a.b.MemberGroup")
                .buildAndConfigure();
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
        final String buildAndConfigureForEnumName = CONFIGURE_FOR_STORAGE_DISABLED_CLIENT.name();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder()
                .setExceptionReporterInstanceClassName(expectedExceptionReportInstanceClassName)
                .setCallbackHandlerInstanceClassName(expectedCallbackHandlerInstanceClassName)
                .setCustomConfiguredCount(expectedCustomConfiguredMemberCount)
                .setStorageEnabledCount(expectedStorageEnabledCount)
                .setStorageEnabledExtendProxyCount(expectedStorageEnabledProxyCount)
                .setExtendProxyCount(expectedExtendProxyCount)
                .setJmxMonitorCount(expectedJmxMonitorCount)
                //
                .setNumberOfThreadsInStartUpPool(expectedNumberOfThreads)
                .setClusterMemberInstanceClassName(expectedInstanceClassName)
                .setClusterMemberGroupInstanceClassName(expectedGroupInstanceClassName)
                .setCustomConfiguredClusterMemberInstanceClassName(expectedCustomConfiguredInstanceClassName)
                //
                .setSuggestedSleepAfterStopDuration35x(expectedSleepDuration35x)
                .setSuggestedSleepAfterStopDuration36x(expectedSleepDuration36x)
                .setSuggestedSleepAfterStopDurationDefault(expectedSleepDurationDefault)
                //
                .setJarsToExcludeFromClassPath(jarsToExcludeFromClassPath)
                .setCoreJarsToExcludeFromClassPath(coreJarsToExcludeFromClassPath)
                //
                .setAppConsoleClassName(appConsoleClassName)
                .setBuildAndConfigureForEnumName(buildAndConfigureForEnumName);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderKeysAndValues();

        final int numberOfSpecificLogLevelsNotSetAndSoOmitted = 4;

        assertThat(builderSettings.size(), is(
                EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE - numberOfSpecificLogLevelsNotSetAndSoOmitted));

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
        final String expectedStorageEnabledCacheConfiguration = "storage-enabled-cache-configuration.xml";
        final String expectedExtendProxyCacheConfiguration = "extend-proxy-cache-configuration.xml";
        final String expectedJmxMonitorCacheConfiguration = "jmx-monitor-cache-configuration.xml";

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
        final int expectedClientLogLevel = 7;
        final int expectedStorageEnabledLogLevel = 6;
        final int expectedExtendProxyLogLevel = 5;
        final int expectedJmxMonitorLogLevel = 4;

        final int expectedFastStartJoinTimeoutMilliseconds = 231;

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setCacheConfiguration(expectedCacheConfiguration);
        builder.setClientCacheConfiguration(expectedClientCacheConfiguration);
        builder.setStorageEnabledCacheConfiguration(expectedStorageEnabledCacheConfiguration);
        builder.setExtendProxyCacheConfiguration(expectedExtendProxyCacheConfiguration);
        builder.setJmxMonitorCacheConfiguration(expectedJmxMonitorCacheConfiguration);
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
        builder.setClientLogLevel(expectedClientLogLevel);
        builder.setStorageEnabledLogLevel(expectedStorageEnabledLogLevel);
        builder.setExtendProxyLogLevel(expectedExtendProxyLogLevel);
        builder.setJmxMonitorLogLevel(expectedJmxMonitorLogLevel);

        builder.setFastStartJoinTimeoutMilliseconds(expectedFastStartJoinTimeoutMilliseconds);


        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Map<String, String> builderSettings = defaultBuilder.getBuilderKeysAndValues();

        final int numberOfNonCoherencePropertiesNotSet = 1; // Presently only JARs to exclude

        assertThat(builderSettings.size(), is(
                EXPECTED_BUILDER_DEFAULT_PROPERTIES_SIZE - numberOfNonCoherencePropertiesNotSet));


        assertThat(builderSettings.get(CACHE_CONFIGURATION_KEY), is(expectedCacheConfiguration));

        assertThat(builderSettings.get(CLIENT_CACHE_CONFIGURATION_KEY), is(expectedClientCacheConfiguration));
        assertThat(builder.getClientCacheConfiguration(), is(expectedClientCacheConfiguration));

        assertThat(builderSettings.get(OVERRIDE_CONFIGURATION_KEY), is(expectedOverrideConfiguration));
        assertThat(builderSettings.get(CLIENT_OVERRIDE_CONFIGURATION_KEY), is(expectedClientOverrideConfiguration));
        assertThat(builderSettings.get(CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY), is(expectedCustomConfiguredCacheConfiguration));
        assertThat(builderSettings.get(STORAGE_ENABLED_CACHE_CONFIGURATION_KEY), is(expectedStorageEnabledCacheConfiguration));
        assertThat(builderSettings.get(EXTEND_PROXY_CACHE_CONFIGURATION_KEY), is(expectedExtendProxyCacheConfiguration));
        assertThat(builderSettings.get(JMX_MONITOR_CACHE_CONFIGURATION_KEY), is(expectedJmxMonitorCacheConfiguration));

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

        assertThat(builderSettings.get(LOG_DESTINATION_KEY), is(expectedLogDestination));
        assertThat(builderSettings.get(LOG_LEVEL_KEY), is(Integer.toString(expectedLogLevel)));
        assertThat(builderSettings.get(CLIENT_LOG_LEVEL_KEY), is(Integer.toString(expectedClientLogLevel)));
        assertThat(builderSettings.get(STORAGE_ENABLED_LOG_LEVEL_KEY), is(Integer.toString(expectedStorageEnabledLogLevel)));
        assertThat(builderSettings.get(EXTEND_PROXY_LOG_LEVEL_KEY), is(Integer.toString(expectedExtendProxyLogLevel)));
        assertThat(builderSettings.get(JMX_MONITOR_LOG_LEVEL_KEY), is(Integer.toString(expectedJmxMonitorLogLevel)));

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
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        return (DefaultClusterMemberGroupBuilder) builder;
    }

    @Test
    public void performToString() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

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
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageEnabled();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForStorageEnabledWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedLogLevel = "-1";

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setStorageEnabledCacheConfiguration(expectedCacheConfiguration);
        builder.setStorageEnabledLogLevel(Integer.parseInt(expectedLogLevel));

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageEnabled();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), is(expectedCacheConfiguration));
        assertThat(properties.getProperty(COHERENCE_LOG_LEVEL_KEY), is(expectedLogLevel));
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyWhenNoSpecificSettings() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxy(123);

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedLogLevel = "-1";

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setExtendProxyCacheConfiguration(expectedCacheConfiguration);
        builder.setExtendProxyLogLevel(Integer.parseInt(expectedLogLevel));

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxy(123);

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), is(expectedCacheConfiguration));
        assertThat(properties.getProperty(COHERENCE_LOG_LEVEL_KEY), is(expectedLogLevel));
    }

    @Test
    @Ignore
    public void defaultMappingSystemPropertiesForStorageEnabledExtendProxy() {

    }

    @Test
    public void defaultMappingSystemPropertiesForStorageDisabledClientNoSpecificSettings() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageDisabledClient();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForStorageDisabledClientSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedOverrideConfiguration = "override-config.xml";
        final String expectedLogLevel = "-1";

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setClientCacheConfiguration(expectedCacheConfiguration);
        builder.setClientOverrideConfiguration(expectedOverrideConfiguration);
        builder.setClientLogLevel(Integer.parseInt(expectedLogLevel));

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForStorageDisabledClient();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), is(expectedCacheConfiguration));
        assertThat(properties.getProperty(COHERENCE_OVERRIDE_KEY), is(expectedOverrideConfiguration));
        assertThat(properties.getProperty(COHERENCE_LOG_LEVEL_KEY), is(expectedLogLevel));
    }

    @Test
    public void defaultMappingSystemPropertiesForJmxMonitorClientWhenNoSpecificSettings() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForJmxMonitor();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForJmxMonitorClientWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedLogLevel = "-1";

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setJmxMonitorCacheConfiguration(expectedCacheConfiguration);
        builder.setJmxMonitorLogLevel(Integer.parseInt(expectedLogLevel));

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForJmxMonitor();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), is(expectedCacheConfiguration));
        assertThat(properties.getProperty(COHERENCE_LOG_LEVEL_KEY), is(expectedLogLevel));
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyClientWhenNoSpecificSettings() {
        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxyClient();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), nullValue());
    }

    @Test
    public void defaultMappingSystemPropertiesForExtendProxyClientWhenSpecificSettings() {
        final String expectedCacheConfiguration = "cache-config.xml";
        final String expectedOverrideConfiguration = "override-config.xml";
        final String expectedLogLevel = "-1";

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        builder.setClientCacheConfiguration(expectedCacheConfiguration);
        builder.setClientOverrideConfiguration(expectedOverrideConfiguration);
        builder.setClientLogLevel(Integer.parseInt(expectedLogLevel));

        final DefaultClusterMemberGroupBuilder defaultBuilder = (DefaultClusterMemberGroupBuilder) builder;
        final Properties properties = defaultBuilder.getSystemPropertiesForExtendProxyClient();

        assertThat(properties.getProperty(COHERENCE_CACHE_CONFIG_KEY), is(expectedCacheConfiguration));
        assertThat(properties.getProperty(COHERENCE_OVERRIDE_KEY), is(expectedOverrideConfiguration));
        assertThat(properties.getProperty(COHERENCE_LOG_LEVEL_KEY), is(expectedLogLevel));
    }

    @Test(expected = UnsupportedOperationException.class)
    public void mergeWhenAllShutdown() {
        final ClusterMemberGroup memberGroup = getClusterMemberGroup();

        ((DefaultClusterMemberGroup) memberGroup).startAll();
        memberGroup.shutdownAll();

        memberGroup.merge(getClusterMemberGroup());
    }

    @Test
    public void registryGetWhenEntryDoesNotExist() {
        final Registry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        assertThat(registry.reusableClusterMemberGroupMap.size(), is(0));
        assertThat(registry.getClusterMemberGroup(builder), nullValue());
    }

    private Registry getRegistryAndClearContents() {
        final Registry registry = Registry.getInstance();

        registry.reusableClusterMemberGroupMap.clear();

        return registry;
    }

    @Test
    public void registryGetWhenEntryDoesExist() {
        final Registry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        registry.registerClusterMemberGroup(builder, getClusterMemberGroup());

        assertThat(registry.reusableClusterMemberGroupMap.size(), is(1));
        assertThat(registry.getClusterMemberGroup(builder), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesNotExist() {
        final Registry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        registry.registerClusterMemberGroup(builder, getClusterMemberGroup());
        assertThat(registry.reusableClusterMemberGroupMap.size(), is(1));
        assertThat(registry.getClusterMemberGroup(builder), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesExist() {
        final Registry registry = getRegistryAndClearContents();

        {
            final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

            final ReusableClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(builder, memberGroup);

            assertThat(registry.reusableClusterMemberGroupMap.size(), is(1));
            assertThat(registry.getClusterMemberGroup(builder), notNullValue());
            assertThat(registry.getClusterMemberGroup(builder), is(memberGroup));
        }

        {
            final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

            final ReusableClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(builder, memberGroup);

            assertThat(registry.reusableClusterMemberGroupMap.size(), is(1));
            assertThat(registry.getClusterMemberGroup(builder), notNullValue());
            assertThat(registry.getClusterMemberGroup(builder), is(memberGroup));
        }
    }

    private ReusableClusterMemberGroup getClusterMemberGroup() {
        return new UsageCountingClusterMemberGroup(new DefaultCallbackHandler(), 0, 0, 0, 0, 0);
    }
}
