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

import org.littlegrid.BuildAndConfigureEnum;
import org.littlegrid.BuildExceptionReporter;
import org.littlegrid.CallbackHandler;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupBuildException;
import org.littlegrid.ClusterMemberGroupBuilder;
import org.littlegrid.ReusableClusterMemberGroup;
import org.littlegrid.ReusableClusterMemberGroupRegistry;
import org.littlegrid.support.BeanUtils;
import org.littlegrid.support.ClassPathUtils;
import org.littlegrid.support.PropertiesUtils;
import org.littlegrid.support.SystemUtils;

import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_EXTEND_CLIENT;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_NO_CLIENT;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_STORAGE_DISABLED_CLIENT;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_STORAGE_ENABLED_MEMBER;

/**
 * Default cluster member group builder implementation.
 */
public class DefaultClusterMemberGroupBuilder implements ClusterMemberGroupBuilder {
    private static final String DEFAULT_PROPERTIES_FILENAME = "littlegrid/littlegrid-builder-default.properties";
    private static final String SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME = "littlegrid/littlegrid-builder-system-property-mapping-default.properties";
    private static final String OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";
    private static final String SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-system-property-mapping-override.properties";
    private static final String FAST_START_OVERRIDE_CONFIGURATION_FILENAME = "littlegrid/littlegrid-fast-start-coherence-override.xml";
    private static final String LITTLEGRID_DIRECTORY_SLASH = "littlegrid/";

    private static final String EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY = "ExceptionReporterInstanceClassName";
    private static final String CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY = "CallbackHandlerInstanceClassName";

    private static final String CUSTOM_CONFIGURED_COUNT_KEY = "CustomConfiguredCount";
    private static final String STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String JMX_MONITOR_COUNT_KEY = "JmxMonitorCount";

    private static final String NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "CustomConfiguredClusterMemberInstanceClassName";
    private static final String CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME = "ClusterMemberGroupInstanceClassName";

    private static final String SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY = "SuggestedSleepAfterStopDurationDefault";

    private static final String CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String STORAGE_ENABLED_CACHE_CONFIGURATION_KEY = "StorageEnabledCacheConfiguration";
    private static final String EXTEND_PROXY_CACHE_CONFIGURATION_KEY = "ExtendProxyCacheConfiguration";
    private static final String JMX_MONITOR_CACHE_CONFIGURATION_KEY = "JmxMonitorCacheConfiguration";
    private static final String OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";
    private static final String CLIENT_OVERRIDE_CONFIGURATION_KEY = "ClientOverrideConfiguration";

    private static final String CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY = "CustomConfiguredCacheConfiguration";
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
    private static final String CLIENT_LOG_LEVEL_KEY = "ClientLogLevel";
    private static final String STORAGE_ENABLED_LOG_LEVEL_KEY = "StorageEnabledLogLevel";
    private static final String EXTEND_PROXY_LOG_LEVEL_KEY = "ExtendProxyLogLevel";
    private static final String JMX_MONITOR_LOG_LEVEL_KEY = "JmxMonitorLogLevel";

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

    private static final String POF_ENABLED = "PofEnabled";
    private static final String POF_CONFIGURATION = "PofConfiguration";

    private static final String LEGACY_ENVIRONMENT_VARIABLE_OR_SYSTEM_PROPERTY_PREFIX_KEY = BUILDER_OVERRIDE_KEY + ".";

    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());

    private final Map<String, String> builderKeysAndValues = new HashMap<>();
    private final Properties additionalSystemProperties = new Properties();
    private final Properties builderKeyToSystemPropertyNameMapping = new Properties();
    private final ReusableClusterMemberGroupRegistry registry = DefaultReusableClusterMemberGroupRegistry.getInstance();

    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        final Map<String, Integer> builderKeysAndValuesLoadedSummary = new LinkedHashMap<>();
        final Map<String, Integer> systemPropertyNameMappingLoadedSummary = new LinkedHashMap<>();

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary);
        loadBuilderKeyToSystemPropertyNameMapping(systemPropertyNameMappingLoadedSummary);

        LOGGER.info(format("___ %s %s (%s) - initialised.  Builder values: %s.  "
                        + "Builder to Coherence system property mapping values: %s ___",
                Info.getName(), Info.getVersionNumber(), Info.getWebsiteAddress(),
                builderKeysAndValuesLoadedSummary, systemPropertyNameMappingLoadedSummary));

    }

    /**
     * Returns the current builder keys and values with their internal builder keys.
     *
     * @return builder keys and values.
     */
    Map<String, String> getBuilderKeysAndValues() {
        return builderKeysAndValues;
    }

    private void loadAndSetBuilderKeysAndValues(final Map<String, Integer> builderKeysAndValuesLoadedSummary) {
        loadAndSetBuilderKeysAndValuesUsingPropertiesFiles(builderKeysAndValuesLoadedSummary);

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "environment variables",
                BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY, SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "environment variables",
                BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY.toUpperCase(), SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "system properties",
                BUILDER_SYSTEM_PROPERTY_PREFIX_KEY, System.getProperties());
    }

    private void loadAndSetBuilderKeysAndValues(final Map<String, Integer> builderKeysAndValuesLoadedSummary,
                                                final String propertiesDescription,
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

        LOGGER.fine(format("Prefixed '%s' %s found: %d", prefixUsed, propertiesDescription, builderOverrides.size()));
        builderKeysAndValuesLoadedSummary.put(format("'%s' %s", preferredPrefix, propertiesDescription),
                builderOverrides.size());

        BeanUtils.multiSetter(this, builderOverrides);
    }

    private void loadAndSetBuilderKeysAndValuesUsingPropertiesFiles(
            final Map<String, Integer> builderKeysAndValuesLoadedSummary) {

        final Properties defaultProperties = PropertiesUtils.loadProperties(Level.FINE, DEFAULT_PROPERTIES_FILENAME);

        BeanUtils.multiSetter(this, defaultProperties);
        builderKeysAndValuesLoadedSummary.put("default file", defaultProperties.size());

        final String alternativePropertiesFilename = System.getProperty(BUILDER_OVERRIDE_KEY);
        final Properties overrideProperties;

        // Check if an alternative properties file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFilename)) {
            overrideProperties = PropertiesUtils.loadProperties(Level.FINE, alternativePropertiesFilename);
        } else {
            overrideProperties = PropertiesUtils.loadProperties(Level.FINE,
                    OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + OVERRIDE_PROPERTIES_FILENAME);
        }

        BeanUtils.multiSetter(this, overrideProperties);
        builderKeysAndValuesLoadedSummary.put("override file", overrideProperties.size());
    }

    private void loadBuilderKeyToSystemPropertyNameMapping(
            final Map<String, Integer> systemPropertyNameMappingLoadedSummary) {

        final Properties defaultMappingProperties =
                PropertiesUtils.loadProperties(Level.FINE, SYSTEM_PROPERTY_MAPPING_DEFAULT_PROPERTIES_FILENAME);
        systemPropertyNameMappingLoadedSummary.put("default file", defaultMappingProperties.size());

        builderKeyToSystemPropertyNameMapping.putAll(defaultMappingProperties);

        final String alternativePropertiesFile = System.getProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY);
        final Properties overrideMappingProperties;

        // Check if an alternative property file should be used, otherwise use standard named override file
        if (stringHasValue(alternativePropertiesFile)) {
            overrideMappingProperties = PropertiesUtils.loadProperties(Level.FINE, alternativePropertiesFile);
        } else {
            overrideMappingProperties = PropertiesUtils.loadProperties(Level.FINE,
                    SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME,
                    LITTLEGRID_DIRECTORY_SLASH + SYSTEM_PROPERTY_MAPPING_OVERRIDE_PROPERTIES_FILENAME);
        }

        builderKeyToSystemPropertyNameMapping.putAll(overrideMappingProperties);
        systemPropertyNameMappingLoadedSummary.put("override file", overrideMappingProperties.size());
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

    void setBuilderValue(final String key,
                         final boolean value) {

        builderKeysAndValues.put(key, Boolean.toString(value));
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
        return buildAndConfigureFor(CONFIGURE_FOR_NO_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageDisabledClient() {
        return buildAndConfigureFor(CONFIGURE_FOR_STORAGE_DISABLED_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForExtendClient() {
        return buildAndConfigureFor(CONFIGURE_FOR_EXTEND_CLIENT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigureForStorageEnabledMember() {
        return buildAndConfigureFor(CONFIGURE_FOR_STORAGE_ENABLED_MEMBER);
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings("unchecked")
    @Override
    public ClusterMemberGroup buildAndConfigureFor(final BuildAndConfigureEnum buildAndConfigureEnum) {
        ClusterMemberGroup memberGroup = getClusterMemberGroupInstance(this);

        final Properties systemProperties;

        switch (buildAndConfigureEnum) {
            case CONFIGURE_FOR_STORAGE_DISABLED_CLIENT:
                systemProperties = getSystemPropertiesForStorageDisabledClient();
                break;

            case CONFIGURE_FOR_EXTEND_CLIENT:
                systemProperties = getSystemPropertiesForExtendProxyClient();
                break;

            case CONFIGURE_FOR_STORAGE_ENABLED_MEMBER:
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

    private ClusterMemberGroup getClusterMemberGroupInstance(final ClusterMemberGroupBuilder builder) {
        final String className = getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME);

        try {
            final Class clusterMemberGroupClass = this.getClass().getClassLoader().loadClass(className);

            if (ReusableClusterMemberGroup.class.isAssignableFrom(clusterMemberGroupClass)) {
                // It is reusable
                ReusableClusterMemberGroup reusableMemberGroup = registry.getClusterMemberGroup(getIdentifier(builder));

                if (reusableMemberGroup == null) {
                    // Whilst it is reusable no instance already exists - create one
                    // Build and register it for later re-use

                    reusableMemberGroup = (ReusableClusterMemberGroup) buildClusterMembers(clusterMemberGroupClass);

                    registry.registerClusterMemberGroup(getIdentifier(builder), reusableMemberGroup);
                } else {
                    // An existing reusable instance has been found, check if it has been shutdown

                    if (reusableMemberGroup.isAllShutdown()) {
                        // Whilst it is reusable and an instance exists, it has been shutdown
                        // and so can't be used.  Build a new one and register it for later re-use

                        reusableMemberGroup = (ReusableClusterMemberGroup)
                                buildClusterMembers(clusterMemberGroupClass);

                        registry.registerClusterMemberGroup(getIdentifier(builder), reusableMemberGroup);
                    }
                }

                return reusableMemberGroup;
            } else {
                // It is not reusable, so create a new one
                return buildClusterMembers(clusterMemberGroupClass);
            }
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(format("Cannot load class '%s", className));
        }
    }

    private String getIdentifier(ClusterMemberGroupBuilder builder) {
        return Integer.toString(builder.hashCode());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigure() {
        return buildAndConfigureFor(Enum.valueOf(BuildAndConfigureEnum.class,
                getBuilderValueAsString(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY)));
    }

    private DefaultClusterMemberGroup buildClusterMembers(final Class clusterMemberGroupClass) {
        final int storageEnabledCount = getBuilderValueAsInt(STORAGE_ENABLED_COUNT_KEY);
        final int customConfiguredCount = getBuilderValueAsInt(CUSTOM_CONFIGURED_COUNT_KEY);
        final int storageEnabledExtendProxyCount = getBuilderValueAsInt(STORAGE_ENABLED_PROXY_COUNT_KEY);
        final int extendProxyCount = getBuilderValueAsInt(EXTEND_PROXY_COUNT_KEY);
        final int jmxMonitorCount = getBuilderValueAsInt(JMX_MONITOR_COUNT_KEY);

        final long startTime = System.currentTimeMillis();
        final BuildExceptionReporter exceptionReporter = createExceptionReporter();

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
        final String clusterMemberGroupInstanceClassName =
                getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME);

        DefaultClusterMemberGroup containerGroup;

        try {
            final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                    javaHome, classPath, pathSeparator,
                    getBuilderValueAsString(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY)
                            + ", " + getBuilderValueAsString(CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY));

            containerGroup = createClusterMemberGroupWithCallbackAndSleepDurations(clusterMemberGroupClass);

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
            exceptionReporter.report(e, builderKeysAndValues, builderKeyToSystemPropertyNameMapping,
                    clusterMemberGroupInstanceClassName, registry.toString());

            throw e;
        } catch (Throwable throwable) {
            exceptionReporter.report(throwable, builderKeysAndValues, builderKeyToSystemPropertyNameMapping,
                    clusterMemberGroupInstanceClassName, registry.toString());

            throw new IllegalStateException(throwable);
        }

        return containerGroup;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExceptionReporterInstanceClassName(
            final String exceptionReportInstanceClassName) {

        setBuilderValue(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY, exceptionReportInstanceClassName);

        return this;
    }

    @SuppressWarnings("unchecked")
    private BuildExceptionReporter createExceptionReporter() {
        final String className = getBuilderValueAsString(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (BuildExceptionReporter) constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s", className));
        }
    }

    @SuppressWarnings("unchecked")
    private CallbackHandler createCallbackHandler() {
        final String className = getBuilderValueAsString(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY);

        try {
            final Class clazz = this.getClass().getClassLoader().loadClass(className);
            final Constructor constructor = clazz.getConstructor();

            return (CallbackHandler) constructor.newInstance();
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

    @SuppressWarnings("unchecked")
    private DefaultClusterMemberGroup createClusterMemberGroupWithCallbackAndSleepDurations(
            final Class clusterMemberGroupClass) {

        final int duration35x = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_35X_KEY);
        final int duration36x = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_36X_KEY);
        final int durationDefault = getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY);
        final int wkaPort = getBuilderValueAsInt(WKA_PORT_KEY);
        final int extendPort = getBuilderValueAsInt(EXTEND_PORT_KEY);

        try {
            final Constructor constructor = clusterMemberGroupClass.getDeclaredConstructor(
                    CallbackHandler.class,
                    int.class, int.class, int.class, int.class, int.class);

            return (DefaultClusterMemberGroup) constructor.newInstance(createCallbackHandler(),
                    duration35x, duration36x, durationDefault, wkaPort, extendPort);
        } catch (Exception e) {
            //TODO:

            throw new UnsupportedOperationException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setClientCacheConfiguration(final String cacheConfiguration) {
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
    public ClusterMemberGroupBuilder setClientOverrideConfiguration(final String overrideConfiguration) {
        setBuilderValue(CLIENT_OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCustomConfiguredCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(CUSTOM_CONFIGURED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setOverrideConfiguration(final String overrideConfiguration) {
        setBuilderValue(OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setAdditionalSystemProperties(final Properties properties) {
        additionalSystemProperties.putAll(properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setAdditionalSystemProperties(final String commaDelimitedPropertiesFilenames) {
        setAdditionalSystemProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    @Override
    public ClusterMemberGroupBuilder setAdditionalSystemProperty(final String key,
                                                                 final String value) {

        additionalSystemProperties.setProperty(key, value);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setAdditionalSystemProperty(final String key,
                                                                 final int value) {

        setAdditionalSystemProperty(key, Integer.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setAdditionalSystemProperty(final String key,
                                                                 final boolean value) {

        setAdditionalSystemProperty(key, Boolean.toString(value));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledCount(final int numberOfMembers) {
        setBuilderValue(STORAGE_ENABLED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(STORAGE_ENABLED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCustomConfiguredCount(final int numberOfMembers) {
        setBuilderValue(CUSTOM_CONFIGURED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(STORAGE_ENABLED_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExtendProxyCount(final int numberOfMembers) {
        setBuilderValue(EXTEND_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExtendProxyCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(EXTEND_PROXY_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setJmxMonitorCount(final int numberOfMembers) {
        setBuilderValue(JMX_MONITOR_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setJmxMonitorCacheConfiguration(final String cacheConfiguration) {
        setBuilderValue(JMX_MONITOR_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setLogDestination(final String logDestination) {
        setBuilderValue(LOG_DESTINATION_KEY, logDestination);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setClusterName(final String clusterName) {
        setBuilderValue(CLUSTER_NAME_KEY, clusterName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setLogLevel(final int logLevel) {
        setBuilderValue(LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setClientLogLevel(final int logLevel) {
        setBuilderValue(CLIENT_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledLogLevel(final int logLevel) {
        setBuilderValue(STORAGE_ENABLED_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExtendProxyLogLevel(final int logLevel) {
        setBuilderValue(EXTEND_PROXY_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setJmxMonitorLogLevel(final int logLevel) {
        setBuilderValue(JMX_MONITOR_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCustomConfiguredRoleName(final String roleName) {
        setBuilderValue(CUSTOM_CONFIGURED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledRoleName(final String roleName) {
        setBuilderValue(STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageEnabledExtendProxyRoleName(final String roleName) {
        setBuilderValue(STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExtendProxyRoleName(final String roleName) {
        setBuilderValue(EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setJmxMonitorRoleName(final String roleName) {
        setBuilderValue(JMX_MONITOR_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setStorageDisabledClientRoleName(final String roleName) {
        setBuilderValue(STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setExtendClientRoleName(final String roleName) {
        setBuilderValue(EXTEND_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        setBuilderValue(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY, clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCustomConfiguredClusterMemberInstanceClassName(
            final String clusterMemberInstanceClassName) {

        setBuilderValue(CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY,
                clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        setBuilderValue(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(jarsToExcludeFromClassPath));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCoreJarsToExcludeFromClassPath(
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
    public ClusterMemberGroupBuilder setWkaAddress(final String wkaAddress) {
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
    public ClusterMemberGroupBuilder setWkaPort(final int wkaPort) {
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
    public ClusterMemberGroupBuilder setExtendPort(final int extendPort) {
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
    public ClusterMemberGroupBuilder setBuilderProperties(final Properties properties) {
        BeanUtils.multiSetter(this, properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setBuilderProperties(final String commaDelimitedPropertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setBuilderProperties(final String... propertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(Level.INFO, propertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setTtl(final int ttl) {
        setBuilderValue(TTL_KEY, ttl);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        setBuilderValue(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY, numberOfThreadsInStartUpPool);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setSuggestedSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_35X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setSuggestedSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_36X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setSuggestedSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        setBuilderValue(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setFastStartJoinTimeoutMilliseconds(final long joinTimeoutMilliseconds) {
        setBuilderValue(FAST_START_JOIN_TIMEOUT_MILLISECONDS, joinTimeoutMilliseconds);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setCallbackHandlerInstanceClassName(
            final String callbackHandlerInstanceClassName) {

        setBuilderValue(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY, callbackHandlerInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setSiteName(final String siteName) {
        setBuilderValue(SITE_NAME_KEY, siteName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setRackName(final String rackName) {
        setBuilderValue(RACK_NAME_KEY, rackName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setMachineName(final String machineName) {
        setBuilderValue(MACHINE_NAME_KEY, machineName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setAppConsoleClassName(final String appConsoleClassName) {
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
    public ClusterMemberGroupBuilder setClusterMemberGroupInstanceClassName(final String clusterMemberGroupInstanceClassName) {
        setBuilderValue(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME, clusterMemberGroupInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setPofEnabled(boolean pofEnabled) {
        setBuilderValue(POF_ENABLED, pofEnabled);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setPofConfiguration(String pofConfiguration) {
        setBuilderValue(POF_CONFIGURATION, pofConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroupBuilder setBuildAndConfigureForEnumName(final String buildAndConfigureForEnumName) {
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

        final DefaultClusterMemberGroupBuilder otherBuilder = (DefaultClusterMemberGroupBuilder) other;

        if (!additionalSystemProperties.equals(otherBuilder.additionalSystemProperties)) {
            return false;
        }

        if (!builderKeyToSystemPropertyNameMapping.equals(otherBuilder.builderKeyToSystemPropertyNameMapping)) {
            return false;
        }

        if (!builderKeysAndValues.equals(otherBuilder.builderKeysAndValues)) {
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
        setPropertyUsingNameMappingAndSuppliedValue(properties, LOCAL_ADDRESS_KEY, getBuilderValueAsString(WKA_ADDRESS_KEY));
        setPropertyUsingNameMappingAndSuppliedValue(properties, EXTEND_ADDRESS_KEY, getBuilderValueAsString(WKA_ADDRESS_KEY));
        setPropertyUsingNameMappingAndBuilderValue(properties, WKA_PORT_KEY);
        setPropertyUsingNameMappingAndSuppliedValue(properties, LOCAL_PORT_KEY, getBuilderValueAsString(WKA_PORT_KEY));

        setPropertyUsingNameMappingAndBuilderValue(properties, TTL_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, CLUSTER_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, SITE_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, RACK_NAME_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, MACHINE_NAME_KEY);

        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_DESTINATION_KEY);
        setPropertyUsingNameMappingAndBuilderValue(properties, LOG_LEVEL_KEY);

        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT, COHERENCE_MANAGEMENT_NONE);
        setPropertyUsingNameMappingAndSuppliedValue(properties, COHERENCE_MANAGEMENT_REMOTE, true);
        setPropertyUsingNameMappingAndSuppliedValue(properties, MANAGEMENT_JMX_REMOTE, false);

        setPropertyUsingNameMappingAndBuilderValue(properties, POF_ENABLED);
        setPropertyUsingNameMappingAndBuilderValue(properties, POF_CONFIGURATION);

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

        setPropertyUsingNameMappingAndBuilderValue(properties, POF_ENABLED);
        setPropertyUsingNameMappingAndBuilderValue(properties, POF_CONFIGURATION);

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
}
