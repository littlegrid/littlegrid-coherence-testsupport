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

import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupBuildException;
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
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.EXTEND_CLIENT;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.NO_CLIENT;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_ENABLED_MEMBER;
import static org.littlegrid.ClusterMemberGroup.Builder;

/**
 * Default cluster member group builder implementation.
 */
public class DefaultClusterMemberGroupBuilder implements Builder {
    private static final String DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-default.properties";

    private static final String SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-system-property-mapping-default.properties";

    private static final String OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";

    private static final String SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME =
            "littlegrid-builder-system-property-mapping-override.properties";

    private static final String FAST_START_OVERRIDE_CONFIGURATION_FILENAME =
            "littlegrid/littlegrid-fast-start-coherence-override.xml";

    private static final String LITTLEGRID_DIRECTORY_SLASH = "littlegrid/";

    private static final String EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY =
            "ExceptionReporterInstanceClassName";

    private static final String CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY =
            "CallbackHandlerInstanceClassName";

    private static final String CUSTOM_CONFIGURED_COUNT_KEY = "CustomConfiguredCount";
    private static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    private static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";
    private static final String CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME = "ClusterMemberGroupInstanceClassName";

    private static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY =
            "SuggestedSleepAfterStopDurationDefault";

    private static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String STORAGE_ENABLED_CACHE_CONFIGURATION_KEY = "StorageEnabledCacheConfiguration";
    private static final String EXTEND_PROXY_CACHE_CONFIGURATION_KEY = "ExtendProxyCacheConfiguration";
    private static final String JMX_MONITOR_CACHE_CONFIGURATION_KEY = "JmxMonitorCacheConfiguration";
    private static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";
    private static final String CLIENT_OVERRIDE_CONFIGURATION_KEY = "ClientOverrideConfiguration";

    private static final String CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY =
            "CustomConfiguredCacheConfiguration";
    private static final String DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    private static final String TCMP_ENABLED_KEY = "TcmpEnabled";

    private static final String EXTEND_ENABLED_KEY = "ExtendEnabled";
    private static final String CLUSTER_NAME_KEY = "ClusterName";
    private static final String SITE_NAME_KEY = "SiteName";
    private static final String RACK_NAME_KEY = "RackName";
    private static final String MACHINE_NAME_KEY = "MachineName";
    private static final String CUSTOM_CONFIGURED_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    private static final String STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    private static final String EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String JMX_MONITOR_ROLE_NAME_KEY = "JmxMonitorRoleName";
    private static final String STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";

    private static final String EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";
    private static final String WKA_PORT_KEY = "WkaPort";
    private static final String LOCAL_ADDRESS_KEY = "LocalAddress";
    private static final String LOCAL_PORT_KEY = "LocalPort";
    private static final String WKA_ADDRESS_KEY = "WkaAddress";
    private static final String EXTEND_ADDRESS_KEY = "ExtendAddress";
    private static final String EXTEND_PORT_KEY = "ExtendPort";

    private static final String TTL_KEY = "Ttl";
    private static final String LOG_DESTINATION_KEY = "LogDestination";

    private static final String LOG_LEVEL_KEY = "LogLevel";
    private static final String JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "JarsToExcludeFromClassPath";

    private static final String CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY = "CoreJarsToExcludeFromClassPath";
    private static final String COHERENCE_MANAGEMENT = "CoherenceManagement";
    private static final String COHERENCE_MANAGEMENT_REMOTE = "CoherenceManagementRemote";

    private static final String MANAGEMENT_JMX_REMOTE = "ManagementJmxRemote";
    private static final String COHERENCE_MANAGEMENT_NONE = "none";

    private static final String COHERENCE_MANAGEMENT_ALL = "all";

    private static final String FAST_START_JOIN_TIMEOUT_MILLISECONDS = "FastStartJoinTimeoutMilliseconds";

    private static final String BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY = "BuildAndConfigureForEnumName";
    private static final String APP_CONSOLE_CLASS_NAME_KEY = "AppConsoleClassName";

    private static final String LEGACY_ENVIRONMENT_VARIABLE_OR_SYSTEM_PROPERTY_PREFIX_KEY = BUILDER_OVERRIDE_KEY + ".";

    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());

    private final Map<String, String> builderKeysAndValues = new HashMap<String, String>();
    private final Properties additionalSystemProperties = new Properties();
    private final Properties builderKeyToSystemPropertyNameMapping = new Properties();

    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        LOGGER.info(format("___ %s %s (%s) - initialising builder ___",
                Info.getName(), Info.getVersionNumber(), "http://littlegrid.bitbucket.org"));

        loadAndSetBuilderKeysAndValues();
        loadBuilderKeyToSystemPropertyNameMapping();
    }

    /**
     * Returns the current builder keys and values with their internal builder keys.
     *
     * @return builder keys and values.
     */
    Map<String, String> getBuilderKeysAndValues() {
        return builderKeysAndValues;
    }

    private void loadAndSetBuilderKeysAndValues() {
        loadAndSetBuilderKeysAndValuesUsingPropertiesFiles();

        loadAndSetBuilderKeysAndValues("environment variables", BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY,
                SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues("environment variables", BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY.toUpperCase(),
                SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues("system properties", BUILDER_SYSTEM_PROPERTY_PREFIX_KEY,
                System.getProperties());
    }

    private void loadAndSetBuilderKeysAndValues(final String propertiesDescription,
                                                final String preferredPrefix,
                                                final Properties environmentVariablesOrSystemProperties) {

        String prefixUsed = LEGACY_ENVIRONMENT_VARIABLE_OR_SYSTEM_PROPERTY_PREFIX_KEY;

        Properties builderOverrides = SystemUtils.getPropertiesWithPrefix(
                environmentVariablesOrSystemProperties, prefixUsed, true);

        if (builderOverrides.size() == 0) {
            prefixUsed = preferredPrefix;

            builderOverrides = SystemUtils.getPropertiesWithPrefix(
                    environmentVariablesOrSystemProperties, preferredPrefix, true);

            // When the prefix littlegrid.builder. is used then it could leave an incorrect key to be applied
            // to the builder if the littlegrid.builder.override system property is specified or if the
            // system property mapping override is specified - simply remove both if they exists
            builderOverrides.remove("override");
            builderOverrides.remove("system.property.mapping.override");
        } else {
            LOGGER.warning(format(
                    "Please note: the preferred prefix for system properties is now '%s' instead of '%s'",
                    preferredPrefix,
                    LEGACY_ENVIRONMENT_VARIABLE_OR_SYSTEM_PROPERTY_PREFIX_KEY));
        }

        LOGGER.info(format("Prefixed '%s' %s found: %d", prefixUsed, propertiesDescription, builderOverrides.size()));

        BeanUtils.multiSetter(this, builderOverrides);
    }

    private void loadAndSetBuilderKeysAndValuesUsingPropertiesFiles() {
        BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.FINE, DEFAULT_PROPERTIES_FILENAME));

        final String alternativePropertiesFilename = System.getProperty(BUILDER_OVERRIDE_KEY);

        // Check if an alternative properties file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFilename)) {
            BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.INFO, alternativePropertiesFilename));
        } else {
            BeanUtils.multiSetter(this, PropertiesUtils.loadProperties(Level.INFO,
                    OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + OVERRIDE_PROPERTIES_FILENAME));
        }
    }

    private void loadBuilderKeyToSystemPropertyNameMapping() {
        builderKeyToSystemPropertyNameMapping.putAll(
                PropertiesUtils.loadProperties(Level.FINE, SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME));

        final String alternativePropertiesFile = System.getProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY);

        // Check if an alternative property file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFile)) {
            builderKeyToSystemPropertyNameMapping.putAll(
                    PropertiesUtils.loadProperties(Level.INFO, alternativePropertiesFile));
        } else {
            builderKeyToSystemPropertyNameMapping.putAll(PropertiesUtils.loadProperties(Level.INFO,
                    SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME));
        }
    }

    private static boolean stringHasValue(final String stringToCheckForValue) {
        return stringToCheckForValue != null && stringToCheckForValue.trim().length() > 0;
    }

    void setBuilderValue(final String key,
                         final int value) {

        builderKeysAndValues.put(key, Integer.toString(value));
    }

    void setBuilderValue(final String key,
                         final long value) {

        builderKeysAndValues.put(key, Long.toString(value));
    }

    void setBuilderValue(final String key,
                         final String value) {

        builderKeysAndValues.put(key, value);
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

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForNoClient() {
        return buildAndConfigureFor(NO_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageDisabledClient() {
        return buildAndConfigureFor(STORAGE_DISABLED_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForExtendClient() {
        return buildAndConfigureFor(EXTEND_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageEnabledMember() {
        return buildAndConfigureFor(STORAGE_ENABLED_MEMBER);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public ClusterMemberGroup buildAndConfigureFor(final BuildAndConfigureEnum buildAndConfigureEnum) {
        ClusterMemberGroup memberGroup = getClusterMemberGroupInstance(this.toString());

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

        final DefaultClusterMemberGroup defaultClusterMemberGroup = (DefaultClusterMemberGroup) memberGroup;
        defaultClusterMemberGroup.startAll();

        return memberGroup;
    }

    private ClusterMemberGroup getClusterMemberGroupInstance(final Object clusterMemberGroupKey) {
        final Registry registry = Registry.getInstance();
        ClusterMemberGroup memberGroup = null;

        try {
            final Class clusterMemberGroupClass = this.getClass().getClassLoader().loadClass(
                    getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME));

            if (ReusableClusterMemberGroup.class.isAssignableFrom(clusterMemberGroupClass)) {
                // It is re-usable
                memberGroup = registry.getClusterMemberGroup(clusterMemberGroupKey);

                if (memberGroup == null) {
                    // Whilst it is re-usable no instance already exists - create one
                    // Build and add to map

                    memberGroup = buildClusterMembers();

                    registry.registerClusterMemberGroup(clusterMemberGroupKey, memberGroup);
                } else {
                    // An existing re-usable instance has been found, check if it has been shutdown

                    if (!memberGroup.isRunning()) {
                        // Whilst it is re-usable and an instance exists, it has been shutdown
                        // and so can't be used.  Build a new one and add it to the map

                        memberGroup = buildClusterMembers();

                        registry.registerClusterMemberGroup(clusterMemberGroupKey, memberGroup);
                    }
                }
            } else {
                // It is not re-usable, so create a new one
                memberGroup = buildClusterMembers();
            }
        } catch (ClassNotFoundException e) {
            //TODO:

            throw new UnsupportedOperationException();
        }
        return memberGroup;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigure() {
        return buildAndConfigureFor(Enum.valueOf(BuildAndConfigureEnum.class,
                getBuilderValueAsString(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY)));
    }

    private DefaultClusterMemberGroup buildClusterMembers() {
        final int storageEnabledCount = getBuilderValueAsInt(STORAGE_ENABLED_COUNT_KEY);
        final int customConfiguredCount = getBuilderValueAsInt(CUSTOM_CONFIGURED_COUNT_KEY);
        final int storageEnabledExtendProxyCount = getBuilderValueAsInt(STORAGE_ENABLED_PROXY_COUNT_KEY);
        final int extendProxyCount = getBuilderValueAsInt(EXTEND_PROXY_COUNT_KEY);
        final int jmxMonitorCount = getBuilderValueAsInt(JMX_MONITOR_COUNT_KEY);

        final long startTime = System.currentTimeMillis();
        final ClusterMemberGroup.BuildExceptionReporter exceptionReporter = createExceptionReporter();

        LOGGER.info(format(
                "___ %s %s starting - Storage-enabled: %d, Extend proxy: %d, Storage-enabled proxy: %d, "
                        + "JMX: %d, Custom configured: %d ___",
                Info.getName(), Info.getVersionNumber(),
                storageEnabledCount, extendProxyCount, storageEnabledExtendProxyCount,
                jmxMonitorCount, customConfiguredCount));

        final int numberOfThreadsInStartUpPool = getBuilderValueAsInt(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY);
        final Properties systemProperties = System.getProperties();
        final String pathSeparator = ClassPathUtils.getPathSeparator(systemProperties);
        final String classPath = ClassPathUtils.getClassPath(systemProperties);
        final String javaHome = ClassPathUtils.getJavaHome(systemProperties);

        DefaultClusterMemberGroup containerGroup = null;

        try {
            final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                    javaHome, classPath, pathSeparator,
                    getBuilderValueAsString(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY)
                            + ", " + getBuilderValueAsString(CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY));

            containerGroup = createDefaultClusterMemberGroupWithCallbackAndSleepDurations();

            buildStorageEnabledMembers(storageEnabledCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildJmxMonitorMembers(jmxMonitorCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildExtendProxyMembers(extendProxyCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildStorageEnabledExtendProxyMembers(storageEnabledExtendProxyCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            buildCustomConfiguredMembers(customConfiguredCount, containerGroup, classPathUrls,
                    numberOfThreadsInStartUpPool);

            final long startDuration = System.currentTimeMillis() - startTime;

            LOGGER.info(format("___ Group of %d cluster member(s) started in %dms, member Ids are: %s ___",
                    containerGroup.getStartedMemberIds().length, startDuration,
                    Arrays.toString(containerGroup.getStartedMemberIds())));
        } catch (ClusterMemberGroupBuildException e) {
            exceptionReporter.report(e, builderKeysAndValues, builderKeyToSystemPropertyNameMapping);

            throw e;
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
    public Builder setExceptionReporterInstanceClassName(
            final String exceptionReportInstanceClassName) {

        setBuilderValue(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY, exceptionReportInstanceClassName);

        return this;
    }

    @SuppressWarnings("unchecked")
    private ClusterMemberGroup.BuildExceptionReporter createExceptionReporter() {
        final String className = getBuilderValueAsString(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY);

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
        final String className = getBuilderValueAsString(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (ClusterMemberGroup.CallbackHandler) constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s", className));
        }
    }

    @SuppressWarnings("unchecked")
    private ClusterMemberGroup createClusterMemberGroup() {
        final String className = getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (ClusterMemberGroup) constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s", className));
        }
    }

    private void buildJmxMonitorMembers(final int jmxMonitorCount,
                                        final DefaultClusterMemberGroup containerGroup,
                                        final URL[] classPathUrls,
                                        final int numberOfThreadsInStartUpPool) {

        final String clusterMemberInstanceClassName =
                getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

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
                getBuilderValueAsString(CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

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
                getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledExtendProxyCount > 0) {
            final int extendStartingPort = getBuilderValueAsInt(EXTEND_PORT_KEY);

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
                getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (extendProxyCount > 0) {
            final int extendStartingPort = getBuilderValueAsInt(EXTEND_PORT_KEY);

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
                getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

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
        final int duration35x = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_35X_KEY);
        final int duration36x = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_36X_KEY);
        final int durationDefault = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY);
        final int wkaPort = getBuilderValueAsInt(WKA_PORT_KEY);
        final int extendPort = getBuilderValueAsInt(EXTEND_PORT_KEY);

        return new DefaultClusterMemberGroup(createCallbackHandler(), duration35x, duration36x,
                durationDefault, wkaPort, extendPort);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClientCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(CLIENT_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getClientCacheConfiguration() {
        return getBuilderValueAsString(CLIENT_CACHE_CONFIGURATION_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClientOverrideConfiguration(final String overrideConfiguration) {
        setBuilderValue(CLIENT_OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setOverrideConfiguration(final String overrideConfiguration) {
        setBuilderValue(OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAdditionalSystemProperties(final Properties properties) {
        additionalSystemProperties.putAll(properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAdditionalSystemProperties(final String commaDelimitedPropertiesFilenames) {
        setAdditionalSystemProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    @Override
    public Builder setAdditionalSystemProperty(final String key,
                                               final String value) {

        additionalSystemProperties.setProperty(key, value);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAdditionalSystemProperty(final String key,
                                               final int value) {

        setAdditionalSystemProperty(key, Integer.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAdditionalSystemProperty(final String key,
                                               final boolean value) {

        setAdditionalSystemProperty(key, Boolean.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledCount(final int numberOfMembers) {
        setBuilderValue(STORAGE_ENABLED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(STORAGE_ENABLED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredCount(final int numberOfMembers) {
        setBuilderValue(CUSTOM_CONFIGURED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(STORAGE_ENABLED_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(EXTEND_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(EXTEND_PROXY_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorCount(final int numberOfMembers) {
        setBuilderValue(JMX_MONITOR_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(JMX_MONITOR_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setLogDestination(final String logDestination) {
        setBuilderValue(LOG_DESTINATION_KEY, logDestination);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterName(final String clusterName) {
        setBuilderValue(CLUSTER_NAME_KEY, clusterName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setLogLevel(final int logLevel) {
        setBuilderValue(LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredRoleName(final String roleName) {
        setBuilderValue(CUSTOM_CONFIGURED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledRoleName(final String roleName) {
        setBuilderValue(STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        setBuilderValue(STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyRoleName(final String roleName) {
        setBuilderValue(EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorRoleName(final String roleName) {
        setBuilderValue(JMX_MONITOR_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageDisabledClientRoleName(final String roleName) {
        setBuilderValue(STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendClientRoleName(final String roleName) {
        setBuilderValue(EXTEND_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        setBuilderValue(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY, clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredClusterMemberInstanceClassName(
            final String clusterMemberInstanceClassName) {

        setBuilderValue(CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY,
                clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        setBuilderValue(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(jarsToExcludeFromClassPath));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCoreJarsToExcludeFromClassPath(
            final String... coreJarsToExcludeFromClassPath) {

        setBuilderValue(CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
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
    public Builder setWkaAddress(final String wkaAddress) {
        setBuilderValue(WKA_ADDRESS_KEY, wkaAddress);

        return this;
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
    public Builder setWkaPort(final int wkaPort) {
        setBuilderValue(WKA_PORT_KEY, wkaPort);

        return this;
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
    public Builder setExtendPort(final int extendPort) {
        setBuilderValue(EXTEND_PORT_KEY, extendPort);

        return this;
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
    public Builder setBuilderProperties(final Properties properties) {
        BeanUtils.multiSetter(this, properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setBuilderProperties(final String commaDelimitedPropertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setBuilderProperties(final String... propertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, propertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setTtl(final int ttl) {
        setBuilderValue(TTL_KEY, ttl);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        setBuilderValue(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY, numberOfThreadsInStartUpPool);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_35X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_36X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setFastStartJoinTimeoutMilliseconds(final long joinTimeoutMilliseconds) {
        setBuilderValue(FAST_START_JOIN_TIMEOUT_MILLISECONDS, joinTimeoutMilliseconds);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCallbackHandlerInstanceClassName(
            final String callbackHandlerInstanceClassName) {

        setBuilderValue(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY, callbackHandlerInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSiteName(final String siteName) {
        setBuilderValue(SITE_NAME_KEY, siteName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setRackName(final String rackName) {
        setBuilderValue(RACK_NAME_KEY, rackName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setMachineName(final String machineName) {
        setBuilderValue(MACHINE_NAME_KEY, machineName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAppConsoleClassName(final String appConsoleClassName) {
        setBuilderValue(APP_CONSOLE_CLASS_NAME_KEY, appConsoleClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getAppConsoleClassName() {
        return getBuilderValueAsString(APP_CONSOLE_CLASS_NAME_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterMemberGroupInstanceClassName(final String clusterMemberGroupInstanceClassName) {
        setBuilderValue(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME, clusterMemberGroupInstanceClassName);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setBuildAndConfigureForEnumName(final String buildAndConfigureForEnumName) {
        setBuilderValue(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY, buildAndConfigureForEnumName);

        return this;
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
     * Returns the system properties that have been configured and will be used for a storage
     * enabled member.
     *
     * @return properties to be applied to system properties.
     */
    Properties getSystemPropertiesForStorageEnabled() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, true);

        final String storageEnabledCacheConfigurationKey = STORAGE_ENABLED_CACHE_CONFIGURATION_KEY;
        final String storageEnabledCacheConfiguration =
                getBuilderValueAsString(storageEnabledCacheConfigurationKey);

        if (stringHasValue(storageEnabledCacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, storageEnabledCacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
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

        final String jmxMonitorCacheConfigurationKey = JMX_MONITOR_CACHE_CONFIGURATION_KEY;
        final String jmxMonitorCacheConfiguration =
                getBuilderValueAsString(jmxMonitorCacheConfigurationKey);

        if (stringHasValue(jmxMonitorCacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, jmxMonitorCacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
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

        final String extendProxyCacheConfigurationKey = EXTEND_PROXY_CACHE_CONFIGURATION_KEY;
        final String extendProxyCacheConfiguration =
                getBuilderValueAsString(extendProxyCacheConfigurationKey);

        if (stringHasValue(extendProxyCacheConfiguration)) {
            setPropertyUsingNameMappingAndBuilderValue(properties, extendProxyCacheConfigurationKey);
        } else {
            setPropertyUsingNameMappingAndBuilderValue(properties, CACHE_CONFIGURATION_KEY);
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

            LOGGER.warning("Fast-start join timeout specified.  Note: the fast-runMain Coherence override file will "
                    + "now be configured to be used");

            setBuilderValue(OVERRIDE_CONFIGURATION_KEY, FAST_START_OVERRIDE_CONFIGURATION_FILENAME);

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

        setPropertyUsingNameMappingAndSuppliedValue(properties, DISTRIBUTED_LOCAL_STORAGE_KEY, false);
        setPropertyUsingNameMappingAndSuppliedValue(properties, TCMP_ENABLED_KEY, false);
        setPropertyUsingNameMappingAndBuilderValue(properties, EXTEND_CLIENT_ROLE_NAME_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ENABLED_KEY, false);
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ADDRESS_KEY,
                getBuilderValueAsString(WKA_ADDRESS_KEY));

        setPropertyUsingNameMappingAndBuilderValue(properties, EXTEND_PORT_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_DESTINATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);

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

    /**
     * Registry containing registered cluster member groups that can be re-used.
     *
     * @since 2.15
     */
    public static class Registry {
        private static final Registry INSTANCE = new Registry();

        /**
         * Default scope to facilitate testing.
         */
        final Map<Object, ClusterMemberGroup> clusterMemberGroupMap =
                new HashMap<Object, ClusterMemberGroup>();

        public static Registry getInstance() {
            return INSTANCE;
        }

        ClusterMemberGroup getClusterMemberGroup(final Object key) {
            return clusterMemberGroupMap.get(key);
        }

        void registerClusterMemberGroup(final Object key,
                                        final ClusterMemberGroup clusterMemberGroup) {

            clusterMemberGroupMap.put(key, clusterMemberGroup);
        }
    }
}
