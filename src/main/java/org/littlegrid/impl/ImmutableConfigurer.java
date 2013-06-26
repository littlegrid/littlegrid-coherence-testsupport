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

import org.littlegrid.support.SystemUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.EXTEND_CLIENT;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_ENABLED_MEMBER;
import static org.littlegrid.ClusterMemberGroup.Configuer;

/**
 * Immutable configurer.
 *
 * @since 2.16
 */
class ImmutableConfigurer implements Configuer {
    static final String FAST_START_OVERRIDE_CONFIGURATION_FILENAME =
            "littlegrid/littlegrid-fast-start-coherence-override.xml";

    static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    static final String STORAGE_ENABLED_CACHE_CONFIGURATION_KEY = "StorageEnabledCacheConfiguration";
    static final String EXTEND_PROXY_CACHE_CONFIGURATION_KEY = "ExtendProxyCacheConfiguration";
    static final String JMX_MONITOR_CACHE_CONFIGURATION_KEY = "JmxMonitorCacheConfiguration";
    static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";
    static final String CLIENT_OVERRIDE_CONFIGURATION_KEY = "ClientOverrideConfiguration";

    static final String CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY =
            "CustomConfiguredCacheConfiguration";

    static final String DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    static final String TCMP_ENABLED_KEY = "TcmpEnabled";

    static final String EXTEND_ENABLED_KEY = "ExtendEnabled";
    static final String CLUSTER_NAME_KEY = "ClusterName";
    static final String SITE_NAME_KEY = "SiteName";
    static final String RACK_NAME_KEY = "RackName";
    static final String MACHINE_NAME_KEY = "MachineName";
    static final String CUSTOM_CONFIGURED_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    static final String STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    static final String STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    static final String EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    static final String JMX_MONITOR_ROLE_NAME_KEY = "JmxMonitorRoleName";
    static final String STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";

    static final String EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";
    static final String WKA_PORT_KEY = "WkaPort";
    static final String LOCAL_ADDRESS_KEY = "LocalAddress";
    static final String LOCAL_PORT_KEY = "LocalPort";
    static final String WKA_ADDRESS_KEY = "WkaAddress";
    static final String EXTEND_ADDRESS_KEY = "ExtendAddress";
    static final String EXTEND_PORT_KEY = "ExtendPort";

    static final String TTL_KEY = "Ttl";
    static final String LOG_DESTINATION_KEY = "LogDestination";

    static final String LOG_LEVEL_KEY = "LogLevel";
    static final String CLIENT_LOG_LEVEL_KEY = "ClientLogLevel";
    static final String STORAGE_ENABLED_LOG_LEVEL_KEY = "StorageEnabledLogLevel";
    static final String EXTEND_PROXY_LOG_LEVEL_KEY = "ExtendProxyLogLevel";
    static final String JMX_MONITOR_LOG_LEVEL_KEY = "JmxMonitorLogLevel";

    static final String COHERENCE_MANAGEMENT = "CoherenceManagement";
    static final String COHERENCE_MANAGEMENT_REMOTE = "CoherenceManagementRemote";

    static final String MANAGEMENT_JMX_REMOTE = "ManagementJmxRemote";
    static final String COHERENCE_MANAGEMENT_NONE = "none";

    static final String COHERENCE_MANAGEMENT_ALL = "all";

    static final String FAST_START_JOIN_TIMEOUT_MILLISECONDS = "FastStartJoinTimeoutMilliseconds";

    static final String EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY =
            "ExceptionReporterInstanceClassName";

    static final String CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY =
            "CallbackHandlerInstanceClassName";

    static final String CUSTOM_CONFIGURED_COUNT_KEY = "CustomConfiguredCount";
    static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    static final String JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    static final String CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";

    static final String CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME = "ClusterMemberGroupInstanceClassName";

    static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY =
            "SuggestedSleepAfterStopDurationDefault";


    static final String JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "JarsToExcludeFromClassPath";

    static final String CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "CoreJarsToExcludeFromClassPath";

    static final String BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY = "BuildAndConfigureForEnumName";
    static final String APP_CONSOLE_CLASS_NAME_KEY = "AppConsoleClassName";

    private final Map<String, String> builderKeysAndValues;
    private final Properties additionalSystemProperties;
    private final Properties builderKeyToSystemPropertyNameMapping;

    private static final Logger LOGGER = Logger.getLogger(ImmutableConfigurer.class.getName());

    /**
     * Constructor.
     *
     * @param builderKeysAndValues       Builder keys and values.
     * @param additionalSystemProperties Additional system properties.
     * @param builderKeyToSystemPropertyNameMapping
     *                                   Builder key to system property name mapping.
     */
    ImmutableConfigurer(final Map<String, String> builderKeysAndValues,
                        final Properties additionalSystemProperties,
                        final Properties builderKeyToSystemPropertyNameMapping) {

        this.builderKeysAndValues = new HashMap<String, String>(builderKeysAndValues);

        this.additionalSystemProperties = new Properties(additionalSystemProperties);
        this.additionalSystemProperties.putAll(additionalSystemProperties);

        this.builderKeyToSystemPropertyNameMapping = new Properties();
        this.builderKeyToSystemPropertyNameMapping.putAll(builderKeyToSystemPropertyNameMapping);
    }

    @Override
    public String getClusterName() {
        return getBuilderValueAsString(CLUSTER_NAME_KEY);
    }

    @Override
    public String getExtendAddress() {
        // The Extend address is always the same as WKA address
        return getBuilderValueAsString(WKA_ADDRESS_KEY);
    }

    /**
     * {@inheritDoc}
     */
    public String getClientCacheConfiguration() {
        return getBuilderValueAsString(CLIENT_CACHE_CONFIGURATION_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWkaAddress() {
        return getBuilderValueAsString(WKA_ADDRESS_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return getBuilderValueAsInt(WKA_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendPort() {
        return getBuilderValueAsInt(EXTEND_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void configureForExtendClient() {
        configureFor(EXTEND_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void configureForStorageDisabledClient() {
        configureFor(STORAGE_DISABLED_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void configureForStorageEnabledMember() {
        configureFor(STORAGE_ENABLED_MEMBER);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void configureFor(final BuildAndConfigureEnum buildAndConfigureEnum) {
        final Properties systemProperties;

        switch (buildAndConfigureEnum) {
            case STORAGE_DISABLED_CLIENT:
                systemProperties = getSystemPropertiesForStorageDisabledClient();
                break;

            case EXTEND_CLIENT:
                systemProperties = getSystemPropertiesForExtendProxyClient();
                break;

            case STORAGE_ENABLED_MEMBER:
                systemProperties = getSystemPropertiesForStorageEnabled();
                break;

            default:
                systemProperties = new Properties();
                break;
        }

        SystemUtils.applyToSystemProperties(systemProperties);

        LOGGER.info(format("System properties set for client/member: %s", new TreeMap(systemProperties)));
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * enabled member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForStorageEnabled() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, true);

        final String cacheConfigurationKey = STORAGE_ENABLED_CACHE_CONFIGURATION_KEY;
        final String cacheConfiguration = getBuilderValueAsString(cacheConfigurationKey);

        if (stringHasValue(cacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, cacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        }

        final String logLevelKey = STORAGE_ENABLED_LOG_LEVEL_KEY;
        final String storageEnabledLogLevel = getBuilderValueAsString(logLevelKey);

        if (stringHasValue(storageEnabledLogLevel)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, logLevelKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);
        }

        setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, STORAGE_ENABLED_ROLE_NAME_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a
     * JMX monitor member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForJmxMonitor() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);

        final String cacheConfigurationKey = JMX_MONITOR_CACHE_CONFIGURATION_KEY;
        final String cacheConfiguration = getBuilderValueAsString(cacheConfigurationKey);

        if (stringHasValue(cacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, cacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        }

        final String logLevelKey = JMX_MONITOR_LOG_LEVEL_KEY;
        final String logLevel = getBuilderValueAsString(logLevelKey);

        if (stringHasValue(logLevel)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, logLevelKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);
        }

        setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, JMX_MONITOR_ROLE_NAME_KEY);

        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT,
                COHERENCE_MANAGEMENT_ALL);

        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT_REMOTE, true);
        setPropertyUsingNameMappingAndSuppliedValue(properties, MANAGEMENT_JMX_REMOTE, true);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a
     * custom configured member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForCustomConfigured() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);

        final String customConfiguredCacheConfiguration =
                getBuilderValueAsString(CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY);

        if (customConfiguredCacheConfiguration.length() == 0) {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY);
        }

        setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, CUSTOM_CONFIGURED_ROLE_NAME_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for an Extend
     * proxy member.
     *
     * @param extendPort Extend port.
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForExtendProxy(final int extendPort) {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);

        final String cacheConfigurationKey = EXTEND_PROXY_CACHE_CONFIGURATION_KEY;
        final String cacheConfiguration = getBuilderValueAsString(cacheConfigurationKey);

        if (stringHasValue(cacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, cacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        }

        final String logLevelKey = EXTEND_PROXY_LOG_LEVEL_KEY;
        final String logLevel = getBuilderValueAsString(logLevelKey);

        if (stringHasValue(logLevel)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, logLevelKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);
        }

        setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, EXTEND_PROXY_ROLE_NAME_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ENABLED_KEY, true);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_PORT_KEY, extendPort);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * enabled Extend proxy member.
     *
     * @param extendPort Extend port.
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForStorageEnabledExtendProxy(final int extendPort) {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, true);
        setPropertyUsingNameMappingAndBuilderValue(properties, STORAGE_ENABLED_PROXY_ROLE_NAME_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ENABLED_KEY, true);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_PORT_KEY, extendPort);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * disabled client member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForStorageDisabledClient() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        final String clientCacheConfiguration = getBuilderValueAsString(CLIENT_CACHE_CONFIGURATION_KEY);

        if (stringHasValue(clientCacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, CLIENT_CACHE_CONFIGURATION_KEY);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
        }

        final String clientOverrideConfiguration = getBuilderValueAsString(CLIENT_OVERRIDE_CONFIGURATION_KEY);

        if (stringHasValue(clientOverrideConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, CLIENT_OVERRIDE_CONFIGURATION_KEY);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, OVERRIDE_CONFIGURATION_KEY);
        }

        final String logLevelKey = CLIENT_LOG_LEVEL_KEY;
        final String logLevel = getBuilderValueAsString(logLevelKey);

        if (stringHasValue(logLevel)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, logLevelKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);
        }

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);
        setPropertyUsingNameMappingAndBuilderValue(properties, STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ENABLED_KEY, false);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private Properties getSystemPropertiesForTcmpClusterMember() {
        final Properties properties = new Properties();

        setPropertyUsingNameMappingAndSuppliedValue(properties, TCMP_ENABLED_KEY, true);
        setPropertyUsingNameMappingAndBuilderValue(properties, WKA_ADDRESS_KEY);

        setPropertyUsingNameMappingAndSuppliedValue(properties, LOCAL_ADDRESS_KEY,
                getBuilderValueAsString(WKA_ADDRESS_KEY));

        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ADDRESS_KEY,
                getBuilderValueAsString(WKA_ADDRESS_KEY));

        setPropertyUsingNameMappingAndBuilderValue(properties, WKA_PORT_KEY);

        setPropertyUsingNameMappingAndSuppliedValue(properties, LOCAL_PORT_KEY,
                getBuilderValueAsString(WKA_PORT_KEY));

        setPropertyUsingNameMappingAndBuilderValue(properties, TTL_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, CLUSTER_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, SITE_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, RACK_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, MACHINE_NAME_KEY);

        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_DESTINATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);

        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT,
                COHERENCE_MANAGEMENT_NONE);

        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT_REMOTE, true);
        setPropertyUsingNameMappingAndSuppliedValue(properties, MANAGEMENT_JMX_REMOTE, false);

        final long fastStartJoinTimeout = getBuilderValueAsLong(FAST_START_JOIN_TIMEOUT_MILLISECONDS);
        final String overrideConfiguration = getBuilderValueAsString(OVERRIDE_CONFIGURATION_KEY);

        if (fastStartJoinTimeout > 0 && (overrideConfiguration == null
                || overrideConfiguration.trim().length() == 0)) {

            LOGGER.warning("Fast-start join timeout specified.  Note: the fast-start Coherence override file will "
                    + "now be configured to be used");

            setPropertyUsingNameMappingAndSuppliedValue(properties, OVERRIDE_CONFIGURATION_KEY,
                    FAST_START_OVERRIDE_CONFIGURATION_FILENAME);

            setPropertyUsingNameMappingAndBuilderValue(properties, FAST_START_JOIN_TIMEOUT_MILLISECONDS);
        }

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for an Extend
     * client member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForExtendProxyClient() {
        final Properties properties = new Properties();

        final String clientCacheConfiguration = getBuilderValueAsString(CLIENT_CACHE_CONFIGURATION_KEY);

        if (stringHasValue(clientCacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, CLIENT_CACHE_CONFIGURATION_KEY);
        } else {
            LOGGER.warning("No client cache configuration has been specified for Extend clients");
        }

        final String clientOverrideConfiguration = getBuilderValueAsString(CLIENT_OVERRIDE_CONFIGURATION_KEY);

        // If client override specified then use it instead of the currently configured override - if one is set
        if (stringHasValue(clientOverrideConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, CLIENT_OVERRIDE_CONFIGURATION_KEY);
        }

        final String logLevelKey = CLIENT_LOG_LEVEL_KEY;
        final String logLevel = getBuilderValueAsString(logLevelKey);

        if (stringHasValue(logLevel)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, logLevelKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);
        }

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);
        setPropertyUsingNameMappingAndSuppliedValue(properties, TCMP_ENABLED_KEY, false);
        setPropertyUsingNameMappingAndBuilderValue(properties, EXTEND_CLIENT_ROLE_NAME_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ENABLED_KEY, false);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ADDRESS_KEY,
                getBuilderValueAsString(WKA_ADDRESS_KEY));

        setPropertyUsingNameMappingAndBuilderValue(properties, EXTEND_PORT_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_DESTINATION_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private void setPropertyUsingNameMappingAndBuilderValue(final Properties properties,
                                                            final String nameMappingAndBuilderKey) {

        setPropertyWhenValid(properties, getPropertyNameFromMapping(nameMappingAndBuilderKey),
                getBuilderValueAsString(nameMappingAndBuilderKey));
    }

    private void setPropertyUsingNameMappingAndSuppliedValue(final Properties properties,
                                                             final String nameMappingKey,
                                                             final String value) {

        setPropertyWhenValid(properties, getPropertyNameFromMapping(nameMappingKey), value);
    }

    private void setPropertyUsingNameMappingAndSuppliedValue(final Properties properties,
                                                             final String nameMappingKey,
                                                             final boolean value) {

        setPropertyWhenValid(properties, getPropertyNameFromMapping(nameMappingKey), Boolean.toString(value));
    }

    private void setPropertyUsingNameMappingAndSuppliedValue(final Properties properties,
                                                             final String nameMappingKey,
                                                             final int value) {

        setPropertyWhenValid(properties, getPropertyNameFromMapping(nameMappingKey), Integer.toString(value));
    }

    String getPropertyNameFromMapping(final String nameMappingKey) {
        final String systemPropertyName = builderKeyToSystemPropertyNameMapping.getProperty(nameMappingKey);

        if (systemPropertyName == null) {
            throw new IllegalStateException(format(
                    "Builder setting key of: '%s' didn't return a mapping to system property name - check "
                            + "mapping of system properties", nameMappingKey));
        }

        return systemPropertyName;
    }


    void setPropertyWhenValid(final Properties properties,
                              final String key,
                              final String value) {

        if (key == null) {
            throw new IllegalArgumentException(format("System property key cannot be null for value of: '%s'", value));
        }

        if (value != null && value.trim().length() > 0) {
            properties.setProperty(key, value);
        }
    }

    @Deprecated //TODO: move this somewhere else
    static boolean stringHasValue(final String stringToCheckForValue) {
        return stringToCheckForValue != null && stringToCheckForValue.trim().length() > 0;
    }

    int getBuilderValueAsInt(final String builderKey) {
        return Integer.parseInt(builderKeysAndValues.get(builderKey));
    }

    long getBuilderValueAsLong(final String builderKey) {
        return Long.parseLong(builderKeysAndValues.get(builderKey));
    }

    String getBuilderValueAsString(final String builderKey) {
        return builderKeysAndValues.get(builderKey);
    }

    public String getAppConsoleClassName() {
        return getBuilderValueAsString(APP_CONSOLE_CLASS_NAME_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return format("Builder{builderKeysAndValues=%s, additionalSystemProperties=%s, "
                + "builderKeyToSystemPropertyNameMapping=%s}",
                builderKeysAndValues, additionalSystemProperties, builderKeyToSystemPropertyNameMapping);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }

        if (other == null || getClass() != other.getClass()) {
            return false;
        }

        final ImmutableConfigurer otherContext = (ImmutableConfigurer) other;

        if (!additionalSystemProperties.equals(otherContext.additionalSystemProperties)) {
            return false;
        }

        if (!builderKeyToSystemPropertyNameMapping.equals(otherContext.builderKeyToSystemPropertyNameMapping)) {
            return false;
        }

        if (!builderKeysAndValues.equals(otherContext.builderKeysAndValues)) {
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        int result = builderKeysAndValues.hashCode();
        result = 31 * result + additionalSystemProperties.hashCode();
        result = 31 * result + builderKeyToSystemPropertyNameMapping.hashCode();

        return result;
    }

    @Deprecated //TODO: not really deprecated but needs to move somewhere else
    static String stringArrayToCommaDelimitedString(final String[] jarsToExcludeFromClassPath) {
        final StringBuilder sb = new StringBuilder();

        int count = 0;

        for (final String jarToExcludeFromClassPath : jarsToExcludeFromClassPath) {
            if (count > 0) {
                sb.append(",");
            }

            sb.append(jarToExcludeFromClassPath);
            count++;
        }

        return sb.toString();
    }

    Map<String, String> getBuilderKeysAndValues() {
        return new HashMap<String, String>(builderKeysAndValues);
    }

    Properties getAdditionalSystemProperties() {
        final Properties properties = new Properties();
        properties.putAll(additionalSystemProperties);

        return properties;
    }

    Properties getBuilderKeyToSystemPropertyNameMapping() {
        final Properties properties = new Properties();
        properties.putAll(builderKeyToSystemPropertyNameMapping);

        return properties;
    }

    protected Properties getDirectMutableAccessToAdditionalSystemProperties() {
        return additionalSystemProperties;
    }

    protected Map<String, String> getDirectMutableAccessToBuilderKeysAndValues() {
        return builderKeysAndValues;
    }

    protected Properties getDirectMutableAccessToBuilderKeyToSystemPropertyNameMapping() {
        return builderKeyToSystemPropertyNameMapping;
    }
}
