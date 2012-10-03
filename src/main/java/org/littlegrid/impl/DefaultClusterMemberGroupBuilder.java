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

package org.littlegrid.impl;

import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.support.BeanUtils;
import org.littlegrid.support.ClassPathUtils;
import org.littlegrid.support.PropertiesUtils;
import org.littlegrid.support.SystemUtils;

import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final String BUILDER_DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-default.properties";

    private static final String BUILDER_SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-system-property-mapping-default.properties";

    private static final String BUILDER_OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";

    private static final String BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME =
            "littlegrid-builder-system-property-mapping-override.properties";

    private static final String FAST_START_OVERRIDE_CONFIGURATION_FILENAME =
            "littlegrid/littlegrid-fast-start-coherence-override.xml";

    private static final String LITTLEGRID_DIRECTORY_SLASH = "littlegrid/";

    private static final String BUILDER_EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY =
            "ExceptionReporterInstanceClassName";

    private static final String BUILDER_CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY =
            "CallbackHandlerInstanceClassName";

    private static final String BUILDER_CUSTOM_CONFIGURED_COUNT_KEY = "CustomConfiguredCount";
    private static final String BUILDER_STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String BUILDER_EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String BUILDER_JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    private static final String BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";

    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY =
            "SuggestedSleepAfterStopDurationDefault";

    private static final String BUILDER_CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String BUILDER_CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String BUILDER_OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";
    private static final String BUILDER_CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY =
            "CustomConfiguredCacheConfiguration";

    private static final String BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    private static final String BUILDER_TCMP_ENABLED_KEY = "TcmpEnabled";
    private static final String BUILDER_EXTEND_ENABLED_KEY = "ExtendEnabled";

    private static final String BUILDER_CLUSTER_NAME_KEY = "ClusterName";
    private static final String BUILDER_SITE_NAME_KEY = "SiteName";
    private static final String BUILDER_RACK_NAME_KEY = "RackName";
    private static final String BUILDER_MACHINE_NAME_KEY = "MachineName";
    private static final String BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    private static final String BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    private static final String BUILDER_EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String BUILDER_JMX_MONITOR_ROLE_NAME_KEY = "JmxMonitorRoleName";
    private static final String BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
    private static final String BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";

    private static final String BUILDER_WKA_PORT_KEY = "WkaPort";
    private static final String BUILDER_LOCAL_ADDRESS_KEY = "LocalAddress";
    private static final String BUILDER_LOCAL_PORT_KEY = "LocalPort";
    private static final String BUILDER_WKA_ADDRESS_KEY = "WkaAddress";
    private static final String BUILDER_EXTEND_ADDRESS_KEY = "ExtendAddress";
    private static final String BUILDER_EXTEND_PORT_KEY = "ExtendPort";
    private static final String BUILDER_TTL_KEY = "Ttl";

    private static final String BUILDER_LOG_DESTINATION_KEY = "LogDestination";
    private static final String BUILDER_LOG_LEVEL_KEY = "LogLevel";

    private static final String BUILDER_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "JarsToExcludeFromClassPath";
    private static final String BUILDER_CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "CoreJarsToExcludeFromClassPath";

    private static final String BUILDER_COHERENCE_MANAGEMENT = "CoherenceManagement";
    private static final String BUILDER_COHERENCE_MANAGEMENT_REMOTE = "CoherenceManagementRemote";
    private static final String BUILDER_MANAGEMENT_JMX_REMOTE = "ManagementJmxRemote";

    private static final String COHERENCE_MANAGEMENT_NONE = "none";
    private static final String COHERENCE_MANAGEMENT_ALL = "all";

    private static final String BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS = "FastStartJoinTimeoutMilliseconds";

    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());

    private Map<String, String> builderKeysAndValues = new HashMap<String, String>();
    private Properties additionalSystemProperties = new Properties();
    private Properties builderKeyToSystemPropertyNameMapping = new Properties();


    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        LOGGER.info(format("___ %s %s - initialising builder", Info.getName(), Info.getVersionNumber()) + " ___");

        loadAndSetBuilderKeysAndValues();
        loadBuilderKeyToSystemPropertyNameMapping();
    }

    /**
     * Returns the current builder keys and values with their internal builder keys.
     *
     * @return builder keys and values.
     */
    public Map<String, String> getBuilderKeysAndValues() {
        return builderKeysAndValues;
    }

    private void loadAndSetBuilderKeysAndValues() {
        BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.FINE, BUILDER_DEFAULT_PROPERTIES_FILENAME));

        final String alternativePropertiesFilename = System.getProperty(BUILDER_OVERRIDE_KEY);

        // Check if an alternative properties file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFilename)) {
            BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.INFO, alternativePropertiesFilename));
        } else {
            BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.INFO,
                    BUILDER_OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + BUILDER_OVERRIDE_PROPERTIES_FILENAME));
        }

        final String prefix = BUILDER_OVERRIDE_KEY + ".";
        final Properties environmentVariablesBuilderOverrides = SystemUtils.getPropertiesWithPrefix(
                SystemUtils.getEnvironmentVariables(), prefix, true);

        LOGGER.info(format("Prefixed '%s' environment variables found: %s", BUILDER_OVERRIDE_KEY,
                environmentVariablesBuilderOverrides.size()));

        BeanUtils.multiSetter(this, environmentVariablesBuilderOverrides);

        final Properties systemPropertiesBuilderOverrides =
                SystemUtils.getPropertiesWithPrefix(System.getProperties(), prefix, true);

        LOGGER.info(format("Prefixed '%s' system properties found: %s", BUILDER_OVERRIDE_KEY,
                systemPropertiesBuilderOverrides.size()));

        BeanUtils.multiSetter(this, systemPropertiesBuilderOverrides);
    }

    private void loadBuilderKeyToSystemPropertyNameMapping() {
        builderKeyToSystemPropertyNameMapping =
                PropertiesUtils.loadProperties(Level.FINE, BUILDER_SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME);

        final String alternativePropertiesFile = System.getProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY);

        // Check if an alternative property file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFile)) {
            builderKeyToSystemPropertyNameMapping.putAll(
                    PropertiesUtils.loadProperties(Level.INFO, alternativePropertiesFile));
        } else {
            builderKeyToSystemPropertyNameMapping.putAll(PropertiesUtils.loadProperties(Level.INFO,
                    BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME));
        }
    }

    private static boolean stringHasValue(final String stringToCheckForValue) {
        return stringToCheckForValue != null && stringToCheckForValue.trim().length() > 0;
    }

    private void setBuilderValue(final String key,
                                 final int value) {

        builderKeysAndValues.put(key, Integer.toString(value));
    }

    private void setBuilderValue(final String key,
                                 final long value) {

        builderKeysAndValues.put(key, Long.toString(value));
    }

    private void setBuilderValue(final String key,
                                 final String value) {

        builderKeysAndValues.put(key, value);
    }

    private int getBuilderValueAsInt(final String builderKey) {
        return Integer.parseInt(builderKeysAndValues.get(builderKey));
    }

    private long getBuilderValueAsLong(final String builderKey) {
        return Long.parseLong(builderKeysAndValues.get(builderKey));
    }

    private String getBuilderValueAsString(final String builderKey) {
        final Object value = builderKeysAndValues.get(builderKey);

        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            return (String) value;
        } else {
            return value.toString();
        }
    }

    private ClusterMemberGroup build() {
        final int storageEnabledCount = getBuilderValueAsInt(BUILDER_STORAGE_ENABLED_COUNT_KEY);
        final int customConfiguredCount = getBuilderValueAsInt(BUILDER_CUSTOM_CONFIGURED_COUNT_KEY);
        final int storageEnabledExtendProxyCount = getBuilderValueAsInt(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY);
        final int extendProxyCount = getBuilderValueAsInt(BUILDER_EXTEND_PROXY_COUNT_KEY);
        final int jmxMonitorCount = getBuilderValueAsInt(BUILDER_JMX_MONITOR_COUNT_KEY);

        return buildClusterMembers(storageEnabledCount,
                customConfiguredCount, storageEnabledExtendProxyCount, extendProxyCount, jmxMonitorCount);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForNoClient() {
        final ClusterMemberGroup memberGroup = build();

        ((DefaultClusterMemberGroup) memberGroup).startAll();

        return memberGroup;
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageDisabledClient() {
        final ClusterMemberGroup memberGroup = build();
        final Properties systemProperties = getSystemPropertiesForStorageDisabledClient();

        SystemUtils.applyToSystemProperties(systemProperties);

        LOGGER.info(format("System properties set for client: %s", new TreeMap(systemProperties)));

        ((DefaultClusterMemberGroup) memberGroup).startAll();

        return memberGroup;
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public ClusterMemberGroup buildAndConfigureForExtendClient() {
        final ClusterMemberGroup memberGroup = build();
        final Properties systemProperties = getSystemPropertiesForExtendProxyClient();

        SystemUtils.applyToSystemProperties(systemProperties);

        LOGGER.info(format("System properties set for client: %s", new TreeMap(systemProperties)));

        ((DefaultClusterMemberGroup) memberGroup).startAll();

        return memberGroup;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageEnabledMember() {
        final ClusterMemberGroup memberGroup = build();
        final Properties systemProperties = getSystemPropertiesForStorageEnabled();

        SystemUtils.applyToSystemProperties(systemProperties);

        LOGGER.info(format("System properties set for member: %s", new TreeMap(systemProperties)));

        ((DefaultClusterMemberGroup) memberGroup).startAll();

        return memberGroup;
    }

    private DefaultClusterMemberGroup buildClusterMembers(final int storageEnabledCount,
                                                          final int customConfiguredCount,
                                                          final int storageEnabledExtendProxyCount,
                                                          final int extendProxyCount,
                                                          final int jmxMonitorCount) {

        final ClusterMemberGroup.BuildExceptionReporter exceptionReporter = createExceptionReporter();

        LOGGER.info(format(
                "___ %s %s starting - Storage-enabled: %s, Extend proxy: %s, "
                        + "Storage-enabled Extend proxy: %s, "
                        + "Custom configured: %s, JMX monitor: %s ___",
                Info.getName(), Info.getVersionNumber(),
                storageEnabledCount, extendProxyCount,
                storageEnabledExtendProxyCount, customConfiguredCount, jmxMonitorCount));

        final int numberOfThreadsInStartUpPool = getBuilderValueAsInt(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY);
        final Properties systemProperties = System.getProperties();
        final String pathSeparator = ClassPathUtils.getPathSeparator(systemProperties);
        final String classPath = ClassPathUtils.getClassPath(systemProperties);
        final String javaHome = ClassPathUtils.getJavaHome(systemProperties);
        final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                javaHome, classPath, pathSeparator,
                getBuilderValueAsString(BUILDER_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY)
                        + ", " + getBuilderValueAsString(BUILDER_CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY));

        final DefaultClusterMemberGroup containerGroup = createDefaultClusterMemberGroupWithCallbackAndSleepDurations();

        try {
            //TODO: tidy this up, all very similar
            buildStorageEnabledMembers(storageEnabledCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildJmxMonitorMembers(jmxMonitorCount, containerGroup, classPathUrls, numberOfThreadsInStartUpPool);

            buildExtendProxyMembers(extendProxyCount, containerGroup, classPathUrls, numberOfThreadsInStartUpPool);

            buildStorageEnabledExtendProxyMembers(storageEnabledExtendProxyCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildCustomConfiguredMembers(customConfiguredCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            LOGGER.info(format("___ Group of cluster member(s) started, member Ids: %s ___",
                    Arrays.toString(containerGroup.getStartedMemberIds())));
        } catch (Throwable throwable) {
            exceptionReporter.report(throwable, builderKeysAndValues, builderKeyToSystemPropertyNameMapping);

            throw new IllegalStateException(throwable);
        }

        return containerGroup;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExceptionReporterInstanceClassName(
            final String exceptionReportInstanceClassName) {

        setBuilderValue(BUILDER_EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY, exceptionReportInstanceClassName);

        return this;
    }

    @SuppressWarnings("unchecked")
    private ClusterMemberGroup.BuildExceptionReporter createExceptionReporter() {
        final String className = getBuilderValueAsString(BUILDER_EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (ClusterMemberGroup.BuildExceptionReporter) constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s", className));
        }
    }

    @SuppressWarnings("unchecked")
    private ClusterMemberGroup.CallbackHandler createCallbackHandler() {
        final String className = getBuilderValueAsString(BUILDER_CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (ClusterMemberGroup.CallbackHandler) constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s", className));
        }
    }

    private void buildJmxMonitorMembers(final int jmxMonitorCount,
                                        final DefaultClusterMemberGroup containerGroup,
                                        final URL[] classPathUrls,
                                        final int numberOfThreadsInStartUpPool) {

        final String clusterMemberInstanceClassName =
                getBuilderValueAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (jmxMonitorCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            jmxMonitorCount,
                            getSystemPropertiesForJmxMonitor(),
                            classPathUrls,
                            clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool);

            containerGroup.merge(memberFutures);
        }
    }

    private void buildCustomConfiguredMembers(final int customConfiguredCount,
                                              final DefaultClusterMemberGroup containerGroup,
                                              final URL[] classPathUrls,
                                              final int numberOfThreadsInStartUpPool) {

        final String customConfiguredClusterMemberInstanceClassName =
                getBuilderValueAsString(BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (customConfiguredCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            customConfiguredCount,
                            getSystemPropertiesForCustomConfigured(),
                            classPathUrls,
                            customConfiguredClusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool);

            containerGroup.merge(memberFutures);
        }
    }

    private void buildStorageEnabledExtendProxyMembers(final int storageEnabledExtendProxyCount,
                                                       final DefaultClusterMemberGroup containerGroup,
                                                       final URL[] classPathUrls,
                                                       final int numberOfThreadsInStartUpPool) {

        final int singleMember = 1;
        final String clusterMemberInstanceClassName =
                getBuilderValueAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledExtendProxyCount > 0) {
            final int extendStartingPort = getBuilderValueAsInt(BUILDER_EXTEND_PORT_KEY);

            for (int i = 0; i < storageEnabledExtendProxyCount; i++) {
                final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                        DefaultClusterMemberGroup.startClusterMembers(
                                singleMember,
                                getSystemPropertiesForStorageEnabledExtendProxy(extendStartingPort + i),
                                classPathUrls,
                                clusterMemberInstanceClassName,
                                numberOfThreadsInStartUpPool);

                containerGroup.merge(memberFutures);
            }
        }
    }

    private void buildExtendProxyMembers(final int extendProxyCount,
                                         final DefaultClusterMemberGroup containerGroup,
                                         final URL[] classPathUrls,
                                         final int numberOfThreadsInStartUpPool) {

        final int singleMember = 1;
        final String clusterMemberInstanceClassName =
                getBuilderValueAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (extendProxyCount > 0) {
            final int extendStartingPort = getBuilderValueAsInt(BUILDER_EXTEND_PORT_KEY);

            for (int i = 0; i < extendProxyCount; i++) {
                final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                        DefaultClusterMemberGroup.startClusterMembers(
                                singleMember,
                                getSystemPropertiesForExtendProxy(extendStartingPort + i),
                                classPathUrls,
                                clusterMemberInstanceClassName,
                                numberOfThreadsInStartUpPool);

                containerGroup.merge(memberFutures);
            }
        }
    }

    private void buildStorageEnabledMembers(final int storageEnabledCount,
                                            final DefaultClusterMemberGroup containerGroup,
                                            final URL[] classPathUrls,
                                            final int numberOfThreadsInStartUpPool) {

        final String clusterMemberInstanceClassName =
                getBuilderValueAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            storageEnabledCount,
                            getSystemPropertiesForStorageEnabled(),
                            classPathUrls,
                            clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool);

            containerGroup.merge(memberFutures);
        }
    }

    private DefaultClusterMemberGroup createDefaultClusterMemberGroupWithCallbackAndSleepDurations() {
        final int duration35x = getBuilderValueAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY);
        final int duration36x = getBuilderValueAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY);
        final int durationDefault = getBuilderValueAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY);

        return new DefaultClusterMemberGroup(createCallbackHandler(), duration35x, duration36x, durationDefault);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(BUILDER_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClientCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(BUILDER_CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setOverrideConfiguration(final String overrideConfiguration) {
        setBuilderValue(BUILDER_OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
/*
    @Override
    public ClusterMemberGroup.Builder setClientOverrideConfiguration(final String overrideConfiguration) {
        throw new UnsupportedOperationException();
    }
*/

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperties(final Properties properties) {
        additionalSystemProperties.putAll(properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperties(final String commaDelimitedPropertiesFilenames) {
        setAdditionalSystemProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperty(final String key,
                                                                  final String value) {

        additionalSystemProperties.setProperty(key, value);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperty(final String key,
                                                                  final int value) {

        setAdditionalSystemProperty(key, Integer.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperty(final String key,
                                                                  final boolean value) {

        setAdditionalSystemProperty(key, Boolean.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCount(final int numberOfMembers) {
        setBuilderValue(BUILDER_STORAGE_ENABLED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredCount(final int numberOfMembers) {
        setBuilderValue(BUILDER_CUSTOM_CONFIGURED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(BUILDER_EXTEND_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setJmxMonitorCount(final int numberOfMembers) {
        setBuilderValue(BUILDER_JMX_MONITOR_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setLogDestination(final String logDestination) {
        setBuilderValue(BUILDER_LOG_DESTINATION_KEY, logDestination);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterName(final String clusterName) {
        setBuilderValue(BUILDER_CLUSTER_NAME_KEY, clusterName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setLogLevel(final int logLevel) {
        setBuilderValue(BUILDER_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredRoleName(final String roleName) {
        setBuilderValue(BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledRoleName(final String roleName) {
        setBuilderValue(BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        setBuilderValue(BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyRoleName(final String roleName) {
        setBuilderValue(BUILDER_EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setJmxMonitorRoleName(final String roleName) {
        setBuilderValue(BUILDER_JMX_MONITOR_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(final String roleName) {
        setBuilderValue(BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendClientRoleName(final String roleName) {
        setBuilderValue(BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        setBuilderValue(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY, clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredClusterMemberInstanceClassName(
            final String clusterMemberInstanceClassName) {

        setBuilderValue(BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY,
                clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        setBuilderValue(BUILDER_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(jarsToExcludeFromClassPath));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoreJarsToExcludeFromClassPath(
            final String... coreJarsToExcludeFromClassPath) {

        setBuilderValue(BUILDER_CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(coreJarsToExcludeFromClassPath));

        return this;
    }

    private String stringArrayToCommaDelimitedString(final String[] jarsToExcludeFromClassPath) {
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

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaAddress(final String wkaAddress) {
        setBuilderValue(BUILDER_WKA_ADDRESS_KEY, wkaAddress);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWkaAddress() {
        return getBuilderValueAsString(BUILDER_WKA_ADDRESS_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        setBuilderValue(BUILDER_WKA_PORT_KEY, wkaPort);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return getBuilderValueAsInt(BUILDER_WKA_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendPort(final int extendPort) {
        setBuilderValue(BUILDER_EXTEND_PORT_KEY, extendPort);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendPort() {
        return getBuilderValueAsInt(BUILDER_EXTEND_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final Properties properties) {
        BeanUtils.multiSetter(this, properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final String commaDelimitedPropertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final String... propertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, propertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setTtl(final int ttl) {
        setBuilderValue(BUILDER_TTL_KEY, ttl);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        setBuilderValue(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY, numberOfThreadsInStartUpPool);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        setBuilderValue(BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        setBuilderValue(BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        setBuilderValue(BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setFastStartJoinTimeoutMilliseconds(final long joinTimeoutMilliseconds) {
        setBuilderValue(BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS, joinTimeoutMilliseconds);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCallbackHandlerInstanceClassName(
            final String callbackHandlerInstanceClassName) {

        setBuilderValue(BUILDER_CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY, callbackHandlerInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSiteName(final String siteName) {
        setBuilderValue(BUILDER_SITE_NAME_KEY, siteName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setRackName(final String rackName) {
        setBuilderValue(BUILDER_RACK_NAME_KEY, rackName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setMachineName(final String machineName) {
        setBuilderValue(BUILDER_MACHINE_NAME_KEY, machineName);

        return this;
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * enabled member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForStorageEnabled() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.TRUE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a
     * JMX monitor member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForJmxMonitor() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.FALSE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_JMX_MONITOR_ROLE_NAME_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_COHERENCE_MANAGEMENT,
                COHERENCE_MANAGEMENT_ALL);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_COHERENCE_MANAGEMENT_REMOTE,
                Boolean.TRUE.toString());

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_MANAGEMENT_JMX_REMOTE,
                Boolean.TRUE.toString());

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a
     * custom configured member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForCustomConfigured() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.FALSE.toString());

        final String customConfiguredCacheConfiguration =
                getBuilderValueAsString(BUILDER_CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY);

        if (customConfiguredCacheConfiguration.length() == 0) {
            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        } else {
            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY);
        }

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY);

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
    public Properties getSystemPropertiesForExtendProxy(final int extendPort) {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.FALSE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_EXTEND_PROXY_ROLE_NAME_KEY);
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_PORT_KEY, Integer.toString(extendPort));

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
    public Properties getSystemPropertiesForStorageEnabledExtendProxy(final int extendPort) {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.TRUE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY);
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_PORT_KEY, Integer.toString(extendPort));

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * disabled client member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForStorageDisabledClient() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        final String clientCacheConfiguration = getBuilderValueAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (stringHasValue(clientCacheConfiguration)) {
            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        } else {
            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        }

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.FALSE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ENABLED_KEY,
                Boolean.FALSE.toString());

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private Properties getSystemPropertiesForTcmpClusterMember() {
        final Properties properties = new Properties();

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_TCMP_ENABLED_KEY,
                Boolean.TRUE.toString());

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_WKA_ADDRESS_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_LOCAL_ADDRESS_KEY,
                getBuilderValueAsString(BUILDER_WKA_ADDRESS_KEY));

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ADDRESS_KEY,
                getBuilderValueAsString(BUILDER_WKA_ADDRESS_KEY));

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_WKA_PORT_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_LOCAL_PORT_KEY,
                getBuilderValueAsString(BUILDER_WKA_PORT_KEY));

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_TTL_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CLUSTER_NAME_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_SITE_NAME_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_RACK_NAME_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_MACHINE_NAME_KEY);

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_LOG_DESTINATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_LOG_LEVEL_KEY);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_COHERENCE_MANAGEMENT,
                COHERENCE_MANAGEMENT_NONE);

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_COHERENCE_MANAGEMENT_REMOTE,
                Boolean.TRUE.toString());

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_MANAGEMENT_JMX_REMOTE,
                Boolean.FALSE.toString());

        final long fastStartJoinTimeout = getBuilderValueAsLong(BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS);
        final String overrideConfiguration = getBuilderValueAsString(BUILDER_OVERRIDE_CONFIGURATION_KEY);

        if (fastStartJoinTimeout > 0 && (overrideConfiguration == null || overrideConfiguration.trim().length() == 0)) {
            LOGGER.warning("Fast-start join timeout specified.  Note: the fast-start Coherence override file will "
                    + "now be configured to be used");

            setBuilderValue(BUILDER_OVERRIDE_CONFIGURATION_KEY, FAST_START_OVERRIDE_CONFIGURATION_FILENAME);

            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS);
        }

        return properties;
    }

    /**
     * Returns the system properties that have been configured and will be used for an Extend
     * client member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForExtendProxyClient() {
        final Properties properties = new Properties();

        final String clientCacheConfiguration = getBuilderValueAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration == null) {
            LOGGER.warning("No client cache configuration has been specified for Extend clients");
        } else {
            setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        }

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY,
                Boolean.FALSE.toString());

        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_TCMP_ENABLED_KEY, Boolean.FALSE.toString());
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY);
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        setWhenValidUsingNameMappingAndSuppliedValue(properties, BUILDER_EXTEND_ADDRESS_KEY,
                getBuilderValueAsString(BUILDER_WKA_ADDRESS_KEY));

        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_EXTEND_PORT_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_LOG_DESTINATION_KEY);
        setWhenValidUsingNameMappingAndBuilderValue(properties, BUILDER_LOG_LEVEL_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private void setWhenValidUsingNameMappingAndBuilderValue(final Properties properties,
                                                             final String nameMappingAndBuilderKey) {

        setPropertyWhenValid(properties, getSystemPropertyNameFromMapping(nameMappingAndBuilderKey),
                getBuilderValueAsString(nameMappingAndBuilderKey));
    }

    private void setWhenValidUsingNameMappingAndSuppliedValue(final Properties properties,
                                                              final String nameMappingKey,
                                                              final String value) {

        setPropertyWhenValid(properties, getSystemPropertyNameFromMapping(nameMappingKey), value);
    }

    private String getSystemPropertyNameFromMapping(final String nameMappingKey) {
        final String systemPropertyName = builderKeyToSystemPropertyNameMapping.getProperty(nameMappingKey);

        if (systemPropertyName == null) {
            throw new IllegalStateException(format(
                    "Builder setting key of: '%s' didn't return a mapping to system property name - check "
                            + "mapping of system properties", nameMappingKey));
        }

        return systemPropertyName;
    }

    private void setPropertyWhenValid(final Properties properties,
                                      final String key,
                                      final String value) {

        if (key == null) {
            throw new IllegalArgumentException(format("System property key cannot be null for value of: '%s'", value));
        }

        if (value != null && value.trim().length() > 0) {
            properties.setProperty(key, value);
        }
    }
}
