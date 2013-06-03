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
import java.util.LinkedHashMap;
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
import static org.littlegrid.ClusterMemberGroup.BuildExceptionReporter;
import static org.littlegrid.ClusterMemberGroup.Builder;
import static org.littlegrid.ClusterMemberGroup.CallbackHandler;
import static org.littlegrid.ClusterMemberGroup.ReusableClusterMemberGroup;

//TODO: Temporary hack
import static org.littlegrid.impl.DefaultConfigurationContext.*;

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

    private static final String LITTLEGRID_DIRECTORY_SLASH = "littlegrid/";


    private static final String LEGACY_ENVIRONMENT_VARIABLE_OR_SYSTEM_PROPERTY_PREFIX_KEY = BUILDER_OVERRIDE_KEY + ".";

    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());

    private final DefaultConfigurationContext context = new DefaultConfigurationContext();

    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        final Map<String, Integer> builderKeysAndValuesLoadedSummary = new LinkedHashMap<String, Integer>();
        final Map<String, Integer> systemPropertyNameMappingLoadedSummary = new LinkedHashMap<String, Integer>();

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary);
        loadBuilderKeyToSystemPropertyNameMapping(systemPropertyNameMappingLoadedSummary);

        LOGGER.info(format("___ %s %s (%s) - initialised.  Builder values: %s.  "
                + "Builder to Coherence system property mapping values: %s ___",
                Info.getName(), Info.getVersionNumber(), "http://littlegrid.bitbucket.org",
                builderKeysAndValuesLoadedSummary, systemPropertyNameMappingLoadedSummary));
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

        context.builderKeyToSystemPropertyNameMapping.putAll(defaultMappingProperties);

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

        context.builderKeyToSystemPropertyNameMapping.putAll(overrideMappingProperties);
        systemPropertyNameMappingLoadedSummary.put("override file", overrideMappingProperties.size());
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
        ClusterMemberGroup memberGroup = getClusterMemberGroupInstance(this);

        final Properties systemProperties;

        switch (buildAndConfigureEnum) {
            case STORAGE_DISABLED_CLIENT:
                systemProperties = context.getSystemPropertiesForStorageDisabledClient();
                break;

            case EXTEND_CLIENT:
                systemProperties = context.getSystemPropertiesForExtendProxyClient();
                break;

            case STORAGE_ENABLED_MEMBER:
                systemProperties = context.getSystemPropertiesForStorageEnabled();
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

    private ClusterMemberGroup getClusterMemberGroupInstance(final Builder builder) {
        final String className = context.getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME);
        final Registry registry = Registry.getInstance();

        try {
            final Class clusterMemberGroupClass = this.getClass().getClassLoader().loadClass(className);

            if (ReusableClusterMemberGroup.class.isAssignableFrom(clusterMemberGroupClass)) {
                // It is reusable
                ReusableClusterMemberGroup reusableMemberGroup = registry.getClusterMemberGroup(builder);

                if (reusableMemberGroup == null) {
                    // Whilst it is reusable no instance already exists - create one
                    // Build and register it for later re-use

                    reusableMemberGroup = (ReusableClusterMemberGroup) buildClusterMembers(clusterMemberGroupClass);

                    registry.registerClusterMemberGroup(builder, reusableMemberGroup);
                } else {
                    // An existing reusable instance has been found, check if it has been shutdown

                    if (reusableMemberGroup.isAllShutdown()) {
                        // Whilst it is reusable and an instance exists, it has been shutdown
                        // and so can't be used.  Build a new one and register it for later re-use

                        reusableMemberGroup = (ReusableClusterMemberGroup)
                                buildClusterMembers(clusterMemberGroupClass);

                        registry.registerClusterMemberGroup(builder, reusableMemberGroup);
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

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup buildAndConfigure() {
        return buildAndConfigureFor(Enum.valueOf(BuildAndConfigureEnum.class,
                context.getBuilderValueAsString(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY)));
    }

    private DefaultClusterMemberGroup buildClusterMembers(final Class clusterMemberGroupClass) {
        final int storageEnabledCount = context.getBuilderValueAsInt(STORAGE_ENABLED_COUNT_KEY);
        final int customConfiguredCount = context.getBuilderValueAsInt(CUSTOM_CONFIGURED_COUNT_KEY);
        final int storageEnabledExtendProxyCount = context.getBuilderValueAsInt(STORAGE_ENABLED_PROXY_COUNT_KEY);
        final int extendProxyCount = context.getBuilderValueAsInt(EXTEND_PROXY_COUNT_KEY);
        final int jmxMonitorCount = context.getBuilderValueAsInt(JMX_MONITOR_COUNT_KEY);

        final long startTime = System.currentTimeMillis();
        final BuildExceptionReporter exceptionReporter = createExceptionReporter();

        LOGGER.info(format(
                "___ %s %s starting - Storage-enabled: %d, Extend proxy: %d, Storage-enabled proxy: %d, "
                        + "JMX: %d, Custom configured: %d ___",
                Info.getName(), Info.getVersionNumber(),
                storageEnabledCount, extendProxyCount, storageEnabledExtendProxyCount,
                jmxMonitorCount, customConfiguredCount));

        final int numberOfThreadsInStartUpPool = context.getBuilderValueAsInt(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY);
        final Properties systemProperties = System.getProperties();
        final String pathSeparator = ClassPathUtils.getPathSeparator(systemProperties);
        final String classPath = ClassPathUtils.getClassPath(systemProperties);
        final String javaHome = ClassPathUtils.getJavaHome(systemProperties);
        final String clusterMemberGroupInstanceClassName =
                context.getBuilderValueAsString(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME);

        DefaultClusterMemberGroup containerGroup = null;

        try {
            final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                    javaHome, classPath, pathSeparator,
                    context.getBuilderValueAsString(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY)
                            + ", " + context.getBuilderValueAsString(CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY));

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
            exceptionReporter.report(e, context.builderKeysAndValues, context.builderKeyToSystemPropertyNameMapping,
                    clusterMemberGroupInstanceClassName, Registry.getInstance().toString());

            throw e;
        } catch (Throwable throwable) {
            exceptionReporter.report(throwable, context.builderKeysAndValues, context.builderKeyToSystemPropertyNameMapping,
                    clusterMemberGroupInstanceClassName, Registry.getInstance().toString());

            throw new IllegalStateException(throwable);
        }

        return containerGroup;
    }

    @SuppressWarnings("unchecked")
    private BuildExceptionReporter createExceptionReporter() {
        final String className = context.getBuilderValueAsString(EXCEPTION_REPORTER_INSTANCE_CLASS_NAME_KEY);

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
        final String className = context.getBuilderValueAsString(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY);

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
                context.getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (jmxMonitorCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            jmxMonitorCount,
                            context.getSystemPropertiesForJmxMonitor(),
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
                context.getBuilderValueAsString(CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (customConfiguredCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            customConfiguredCount,
                            context.getSystemPropertiesForCustomConfigured(),
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
                context.getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledExtendProxyCount > 0) {
            final int extendStartingPort = context.getBuilderValueAsInt(EXTEND_PORT_KEY);

            for (int i = 0; i < storageEnabledExtendProxyCount; i++) {
                final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                        DefaultClusterMemberGroup.startClusterMembers(
                                singleMember,
                                context.getSystemPropertiesForStorageEnabledExtendProxy(extendStartingPort + i),
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
                context.getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (extendProxyCount > 0) {
            final int extendStartingPort = context.getBuilderValueAsInt(EXTEND_PORT_KEY);

            for (int i = 0; i < extendProxyCount; i++) {
                final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                        DefaultClusterMemberGroup.startClusterMembers(
                                singleMember,
                                context.getSystemPropertiesForExtendProxy(extendStartingPort + i),
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
                context.getBuilderValueAsString(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledCount > 0) {
            final List<Future<DelegatingClusterMemberWrapper>> memberFutures =
                    DefaultClusterMemberGroup.startClusterMembers(
                            storageEnabledCount,
                            context.getSystemPropertiesForStorageEnabled(),
                            classPathUrls,
                            clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool);

            containerGroup.merge(memberFutures);
        }
    }

    @SuppressWarnings("unchecked")
    private DefaultClusterMemberGroup createClusterMemberGroupWithCallbackAndSleepDurations(
            final Class clusterMemberGroupClass) {

        final int duration35x = context.getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_35X_KEY);
        final int duration36x = context.getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_36X_KEY);
        final int durationDefault = context.getBuilderValueAsInt(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY);
        final int wkaPort = context.getBuilderValueAsInt(WKA_PORT_KEY);
        final int extendPort = context.getBuilderValueAsInt(EXTEND_PORT_KEY);

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
    public Builder setExceptionReporterInstanceClassName(final String exceptionReportInstanceClassName) {
        context.setExceptionReporterInstanceClassName(exceptionReportInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCacheConfiguration(final String cacheConfiguration) {
        context.setCacheConfiguration(cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClientCacheConfiguration(final String cacheConfiguration) {
        context.setClientCacheConfiguration(cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getClientCacheConfiguration() {
        return context.getBuilderValueAsString(CLIENT_CACHE_CONFIGURATION_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClientOverrideConfiguration(final String overrideConfiguration) {
        context.setClientOverrideConfiguration(overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredCacheConfiguration(final String cacheConfiguration) {
        context.setCustomConfiguredCacheConfiguration(cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setOverrideConfiguration(final String overrideConfiguration) {
        context.setOverrideConfiguration(overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAdditionalSystemProperties(final Properties properties) {
        context.setAdditionalSystemProperties(properties);

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

        context.setAdditionalSystemProperty(key, value);

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
        context.setBuilderValue(STORAGE_ENABLED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledCacheConfiguration(final String cacheConfiguration) {
        context.setBuilderValue(STORAGE_ENABLED_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredCount(final int numberOfMembers) {
        context.setBuilderValue(CUSTOM_CONFIGURED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        context.setBuilderValue(STORAGE_ENABLED_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyCount(final int numberOfMembers) {
        context.setBuilderValue(EXTEND_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyCacheConfiguration(final String cacheConfiguration) {
        context.setBuilderValue(EXTEND_PROXY_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorCount(final int numberOfMembers) {
        context.setBuilderValue(JMX_MONITOR_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorCacheConfiguration(final String cacheConfiguration) {
        context.setBuilderValue(JMX_MONITOR_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setLogDestination(final String logDestination) {
        context.setBuilderValue(LOG_DESTINATION_KEY, logDestination);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterName(final String clusterName) {
        context.setBuilderValue(CLUSTER_NAME_KEY, clusterName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setLogLevel(final int logLevel) {
        context.setBuilderValue(LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClientLogLevel(final int logLevel) {
        context.setBuilderValue(CLIENT_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledLogLevel(final int logLevel) {
        context.setBuilderValue(STORAGE_ENABLED_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyLogLevel(final int logLevel) {
        context.setBuilderValue(EXTEND_PROXY_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorLogLevel(final int logLevel) {
        context.setBuilderValue(JMX_MONITOR_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredRoleName(final String roleName) {
        context.setBuilderValue(CUSTOM_CONFIGURED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledRoleName(final String roleName) {
        context.setBuilderValue(STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        context.setBuilderValue(STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendProxyRoleName(final String roleName) {
        context.setBuilderValue(EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJmxMonitorRoleName(final String roleName) {
        context.setBuilderValue(JMX_MONITOR_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setStorageDisabledClientRoleName(final String roleName) {
        context.setBuilderValue(STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendClientRoleName(final String roleName) {
        context.setBuilderValue(EXTEND_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        context.setBuilderValue(CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY, clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCustomConfiguredClusterMemberInstanceClassName(
            final String clusterMemberInstanceClassName) {

        context.setBuilderValue(CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY,
                clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        context.setBuilderValue(JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(jarsToExcludeFromClassPath));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCoreJarsToExcludeFromClassPath(
            final String... coreJarsToExcludeFromClassPath) {

        context.setBuilderValue(CORE_JARS_TO_EXCLUDE_FROM_CLASS_PATH_KEY,
                stringArrayToCommaDelimitedString(coreJarsToExcludeFromClassPath));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setWkaAddress(final String wkaAddress) {
        context.setBuilderValue(WKA_ADDRESS_KEY, wkaAddress);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWkaAddress() {
        return context.getBuilderValueAsString(WKA_ADDRESS_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setWkaPort(final int wkaPort) {
        context.setBuilderValue(WKA_PORT_KEY, wkaPort);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return context.getBuilderValueAsInt(WKA_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setExtendPort(final int extendPort) {
        context.setBuilderValue(EXTEND_PORT_KEY, extendPort);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendPort() {
        return context.getBuilderValueAsInt(EXTEND_PORT_KEY);
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
        context.setBuilderValue(TTL_KEY, ttl);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        context.setBuilderValue(NUMBER_OF_THREADS_IN_START_UP_POOL_KEY, numberOfThreadsInStartUpPool);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        context.setBuilderValue(SLEEP_AFTER_STOP_DURATION_35X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        context.setBuilderValue(SLEEP_AFTER_STOP_DURATION_36X_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSuggestedSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        context.setBuilderValue(SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setFastStartJoinTimeoutMilliseconds(final long joinTimeoutMilliseconds) {
        context.setBuilderValue(FAST_START_JOIN_TIMEOUT_MILLISECONDS, joinTimeoutMilliseconds);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setCallbackHandlerInstanceClassName(
            final String callbackHandlerInstanceClassName) {

        context.setBuilderValue(CALLBACK_HANDLER_INSTANCE_CLASS_NAME_KEY, callbackHandlerInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSiteName(final String siteName) {
        context.setBuilderValue(SITE_NAME_KEY, siteName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setRackName(final String rackName) {
        context.setBuilderValue(RACK_NAME_KEY, rackName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setMachineName(final String machineName) {
        context.setBuilderValue(MACHINE_NAME_KEY, machineName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAppConsoleClassName(final String appConsoleClassName) {
        context.setBuilderValue(APP_CONSOLE_CLASS_NAME_KEY, appConsoleClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getAppConsoleClassName() {
        return context.getBuilderValueAsString(APP_CONSOLE_CLASS_NAME_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setClusterMemberGroupInstanceClassName(final String clusterMemberGroupInstanceClassName) {
        context.setBuilderValue(CLUSTER_MEMBER_GROUP_INSTANCE_CLASS_NAME, clusterMemberGroupInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setBuildAndConfigureForEnumName(final String buildAndConfigureForEnumName) {
        context.setBuilderValue(BUILD_AND_CONFIG_FOR_ENUM_NAME_KEY, buildAndConfigureForEnumName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setPofConfiguration(final String pofConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return format("Builder{builderKeysAndValues=%s, additionalSystemProperties=%s, "
                + "builderKeyToSystemPropertyNameMapping=%s}",
                context.builderKeysAndValues, context.additionalSystemProperties, context.builderKeyToSystemPropertyNameMapping);
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

        if (!context.additionalSystemProperties.equals(otherBuilder.context.additionalSystemProperties)) {
            return false;
        }

        if (!context.builderKeyToSystemPropertyNameMapping.equals(otherBuilder.context.builderKeyToSystemPropertyNameMapping)) {
            return false;
        }

        if (!context.builderKeysAndValues.equals(otherBuilder.context.builderKeysAndValues)) {
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        int result = context.builderKeysAndValues.hashCode();
        result = 31 * result + context.additionalSystemProperties.hashCode();
        result = 31 * result + context.builderKeyToSystemPropertyNameMapping.hashCode();

        return result;
    }

    /**
     * Registry containing registered cluster member groups that can be re-used.
     *
     * @since 2.15
     */
    static class Registry {
        private static final Logger LOGGER = Logger.getLogger(Registry.class.getName());

        private static final Registry INSTANCE = new Registry();

        /**
         * Default scope to facilitate testing.
         */
        final Map<Object, ReusableClusterMemberGroup> reusableClusterMemberGroupMap =
                new HashMap<Object, ReusableClusterMemberGroup>();

        static Registry getInstance() {
            return INSTANCE;
        }

        ReusableClusterMemberGroup getClusterMemberGroup(final Builder builder) {
            final Object key = getKey(builder);
            final ReusableClusterMemberGroup memberGroup = reusableClusterMemberGroupMap.get(key);

            LOGGER.info(format("Member group get using key: '%s' returned group: %s", key, memberGroup));

            return memberGroup;
        }

        void registerClusterMemberGroup(final Builder builder,
                                        final ReusableClusterMemberGroup clusterMemberGroup) {

            final Object key = getKey(builder);

            LOGGER.info(format("Member group registered using key: '%s' with group of: %s", key, clusterMemberGroup));
            reusableClusterMemberGroupMap.put(key, clusterMemberGroup);
        }

        private Object getKey(Builder builder) {
            return builder.hashCode();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String toString() {
            return format("Registry: %s", reusableClusterMemberGroupMap);
        }
    }

    @Deprecated
    DefaultConfigurationContext getContext() {
        return context;
    }
}
