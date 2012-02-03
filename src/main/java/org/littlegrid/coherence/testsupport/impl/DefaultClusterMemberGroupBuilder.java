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

import org.littlegrid.coherence.testsupport.ClusterMemberGroup;
import org.littlegrid.utils.BeanUtils;
import org.littlegrid.utils.LoggerPlaceHolder;
import org.littlegrid.utils.PropertiesUtils;
import org.littlegrid.utils.SystemUtils;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static java.lang.String.format;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final String BUILDER_DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-default.properties";

    private static final String BUILDER_DEFAULT_MAPPING_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-builder-system-property-mapping-default.properties";

    private static final String BUILDER_OVERRIDE_SYSTEM_PROPERTY_NAME = "littlegrid.builder.override";
    private static final String BUILDER_OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";

    private static final String BUILDER_CUSTOM_CONFIGURED_COUNT_KEY = "CustomConfiguredCount";
    private static final String BUILDER_STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";
    private static final String BUILDER_EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";

    private static final String BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";
    private static final String BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY =
            "CustomConfiguredClusterMemberInstanceClassName";

    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY = "SuggestedSleepAfterStopDuration35x";
    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY = "SuggestedSleepAfterStopDuration36x";
    private static final String BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY
            = "SuggestedSleepAfterStopDurationDefault";

    private static final String BUILDER_CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String BUILDER_CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String BUILDER_OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";

    private static final String BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    private static final String BUILDER_TCMP_ENABLED_KEY = "TcmpEnabled";
    private static final String BUILDER_EXTEND_ENABLED_KEY = "ExtendEnabled";

    private static final String BUILDER_CLUSTER_NAME_KEY = "ClusterName";
    private static final String BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY = "CustomConfiguredRoleName";
    private static final String BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
    private static final String BUILDER_EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
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

    private static final String BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS = "FastStartJoinTimeoutMilliseconds";

    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(DefaultClusterMemberGroupBuilder.class.getName());

    private Map<String, String> builderSettings = new HashMap<String, String>();

    private Properties additionalSystemProperties = new Properties();
    private Properties builderSystemPropertyMapping = new Properties();

    private String[] jarsToExcludeFromClassPath;
    private URL[] classPathUrls;


    //TODO: littlegrid#5 Think about JMX
//            properties.addSystemProperty(COHERENCE_MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(JMX_MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMX_JMXREMOTE_KEY, "");

    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        loadAndProcessBuilderSettings();
        loadBuilderMappingSettings();
    }

    /**
     * Returns the current builder settings with their internal builder keys.
     *
     * @return builder settings.
     */
    public Map<String, String> getBuilderSettings() {
        return builderSettings;
    }

    private void loadAndProcessBuilderSettings() {
        final String overridePropertiesFilename =
                System.getProperty(BUILDER_OVERRIDE_SYSTEM_PROPERTY_NAME, BUILDER_OVERRIDE_PROPERTIES_FILENAME
                        + ", littlegrid/" + BUILDER_OVERRIDE_PROPERTIES_FILENAME);

        final String propertiesFilenames;

        if (overridePropertiesFilename.trim().isEmpty()) {
            propertiesFilenames = BUILDER_DEFAULT_PROPERTIES_FILENAME;
        } else {
            propertiesFilenames = BUILDER_DEFAULT_PROPERTIES_FILENAME + ", " + overridePropertiesFilename;
        }

        BeanUtils.processProperties(this, PropertiesUtils.loadProperties(propertiesFilenames));
    }

    private void loadBuilderMappingSettings() {
        builderSystemPropertyMapping = PropertiesUtils.loadProperties(BUILDER_DEFAULT_MAPPING_PROPERTIES_FILENAME);
    }

    private int getBuilderSettingAsInt(final String builderKey) {
        return Integer.parseInt(builderSettings.get(builderKey));
    }

    private String getBuilderSettingAsString(final String builderKey) {
        final Object value = builderSettings.get(builderKey);

        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            return (String) value;
        } else {
            return value.toString();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup build() {
        //TODO: littlegrid#6 Tidy this up
        // on exception output: class path, tangosol system properties, all system properties and message
        // to suggest checking for another running cluster

        int storageEnabledCount = getBuilderSettingAsInt(BUILDER_STORAGE_ENABLED_COUNT_KEY);
        int customConfiguredCount = getBuilderSettingAsInt(BUILDER_CUSTOM_CONFIGURED_COUNT_KEY);
        final int extendProxyCount = getBuilderSettingAsInt(BUILDER_EXTEND_PROXY_COUNT_KEY);
        final int storageEnabledExtendProxyCount = getBuilderSettingAsInt(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY);

        if (storageEnabledCount == 0 && storageEnabledExtendProxyCount == 0
                && extendProxyCount == 0 && customConfiguredCount == 0) {

            storageEnabledCount = 1;
        }

        final int numberOfThreadsInStartUpPool = getBuilderSettingAsInt(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY);

        if (classPathUrls == null) {
            LOGGER.debug("Cluster member group config class path URLs null, setting to current (minus Java home)");

            this.classPathUrls = getClassPathUrlsExcludingJavaHome(jarsToExcludeFromClassPath);
        }

        final DefaultClusterMemberGroup containerGroup = createDefaultClusterMemberGroupWithSleepDurations();

        buildStorageEnabledMembers(storageEnabledCount, containerGroup, classPathUrls, numberOfThreadsInStartUpPool);
        buildCustomConfiguredMembers(customConfiguredCount, containerGroup, classPathUrls,
                numberOfThreadsInStartUpPool);

        buildExtendProxyMembers(extendProxyCount, containerGroup, classPathUrls, numberOfThreadsInStartUpPool);
        buildStorageEnabledExtendProxyMembers(storageEnabledExtendProxyCount, containerGroup, classPathUrls,
                numberOfThreadsInStartUpPool);


        // Clear and now prepare system properties based upon the anticipated client type.
        if (storageEnabledExtendProxyCount > 0 || extendProxyCount > 0) {
            SystemUtils.applyToSystemProperties(getSystemPropertiesForExtendProxyClient());
        } else {
            SystemUtils.applyToSystemProperties(getSystemPropertiesForStorageDisabledClient());
        }

        LOGGER.info(format("Coherence system properties for client: %s",
                SystemUtils.getSystemPropertiesWithPrefix("tangosol.coherence.")));

        return containerGroup;
    }

    private void buildCustomConfiguredMembers(final int customConfiguredCount,
                                              final DefaultClusterMemberGroup containerGroup,
                                              final URL[] classPathUrls,
                                              final int numberOfThreadsInStartUpPool) {

        final String customConfiguredClusterMemberInstanceClassName =
                getBuilderSettingAsString(BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (customConfiguredCount > 0) {
            final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(customConfiguredCount,
                    getSystemPropertiesForCustomConfigured(), classPathUrls,
                    customConfiguredClusterMemberInstanceClassName,
                    numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge(memberGroup);
        }
    }

    private void buildStorageEnabledExtendProxyMembers(final int storageEnabledExtendProxyCount,
                                                       final DefaultClusterMemberGroup containerGroup,
                                                       final URL[] classPathUrls,
                                                       final int numberOfThreadsInStartUpPool) {

        final int singleMember = 1;
        final String clusterMemberInstanceClassName =
                getBuilderSettingAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledExtendProxyCount > 0) {
            final int extendStartingPort = getBuilderSettingAsInt(BUILDER_EXTEND_PORT_KEY);

            //TODO: Currently each it launched separately
            for (int i = 0; i < storageEnabledExtendProxyCount; i++) {
                final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(singleMember,
                        getSystemPropertiesForStorageEnabledExtendProxy(extendStartingPort + i),
                        classPathUrls, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                        .startAll();

                containerGroup.merge(memberGroup);
            }
        }
    }

    private void buildExtendProxyMembers(final int extendProxyCount,
                                         final DefaultClusterMemberGroup containerGroup,
                                         final URL[] classPathUrls,
                                         final int numberOfThreadsInStartUpPool) {

        final int singleMember = 1;
        final String clusterMemberInstanceClassName =
                getBuilderSettingAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (extendProxyCount > 0) {
            final int extendStartingPort = getBuilderSettingAsInt(BUILDER_EXTEND_PORT_KEY);

            //TODO: Currently each it launched separately
            for (int i = 0; i < extendProxyCount; i++) {
                final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(singleMember,
                        getSystemPropertiesForExtendProxy(extendStartingPort + i),
                        classPathUrls, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                        .startAll();

                containerGroup.merge(memberGroup);
            }
        }
    }

    private void buildStorageEnabledMembers(final int storageEnabledCount,
                                            final DefaultClusterMemberGroup containerGroup,
                                            final URL[] classPathUrls,
                                            final int numberOfThreadsInStartUpPool) {

        final String clusterMemberInstanceClassName =
                getBuilderSettingAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        if (storageEnabledCount > 0) {
            final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(storageEnabledCount,
                    getSystemPropertiesForStorageEnabled(), classPathUrls, clusterMemberInstanceClassName,
                    numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge(memberGroup);
        }
    }

    private DefaultClusterMemberGroup createDefaultClusterMemberGroupWithSleepDurations() {
        final int duration35x = getBuilderSettingAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY);
        final int duration36x = getBuilderSettingAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY);
        final int durationDefault = getBuilderSettingAsInt(BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY);

        return new DefaultClusterMemberGroup(duration35x, duration36x, durationDefault);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        builderSettings.put(BUILDER_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxySpecificCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledSpecificCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClientCacheConfiguration(final String cacheConfiguration) {
        builderSettings.put(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY, cacheConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setOverrideConfiguration(final String overrideConfiguration) {
        builderSettings.put(BUILDER_OVERRIDE_CONFIGURATION_KEY, overrideConfiguration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClientOverrideConfiguration(final String overrideConfiguration) {
        throw new UnsupportedOperationException();
    }

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
    public ClusterMemberGroup.Builder setAdditionalSystemProperties(final String propertiesFilenames) {
        setAdditionalSystemProperties(PropertiesUtils.loadProperties(propertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_COUNT_KEY, Integer.toString(numberOfMembers));

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_CUSTOM_CONFIGURED_COUNT_KEY, Integer.toString(numberOfMembers));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY, Integer.toString(numberOfMembers));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_EXTEND_PROXY_COUNT_KEY, Integer.toString(numberOfMembers));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setLogDestination(final String logDestination) {
        builderSettings.put(BUILDER_LOG_DESTINATION_KEY, logDestination);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterName(final String clusterName) {
        builderSettings.put(BUILDER_CLUSTER_NAME_KEY, clusterName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setLogLevel(final int logLevel) {
        builderSettings.put(BUILDER_LOG_LEVEL_KEY, Integer.toString(logLevel));

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredRoleName(final String roleName) {
        builderSettings.put(BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyRoleName(final String roleName) {
        builderSettings.put(BUILDER_EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendClientRoleName(final String roleName) {
        builderSettings.put(BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        builderSettings.put(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY, clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCustomConfiguredClusterMemberInstanceClassName(
            final String clusterMemberInstanceClassName) {

        builderSettings.put(BUILDER_CUSTOM_CONFIGURATION_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY,
                clusterMemberInstanceClassName);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        this.jarsToExcludeFromClassPath = jarsToExcludeFromClassPath;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaAddress(final String wkaAddress) {
        builderSettings.put(BUILDER_WKA_ADDRESS_KEY, wkaAddress);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        builderSettings.put(BUILDER_WKA_PORT_KEY, Integer.toString(wkaPort));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return getBuilderSettingAsInt(BUILDER_WKA_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendPort(final int extendPort) {
        builderSettings.put(BUILDER_EXTEND_PORT_KEY, Integer.toString(extendPort));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendPort() {
        return getBuilderSettingAsInt(BUILDER_EXTEND_PORT_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final Properties properties) {
        BeanUtils.processProperties(this, properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final String commaDelimitedPropertiesFilenames) {
        setBuilderProperties(PropertiesUtils.loadProperties(commaDelimitedPropertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setTtl(final int ttl) {
        builderSettings.put(BUILDER_TTL_KEY, Integer.toString(ttl));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        builderSettings.put(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY,
                Integer.toString(numberOfThreadsInStartUpPool));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        builderSettings.put(BUILDER_SLEEP_AFTER_STOP_DURATION_35X_KEY, Integer.toString(sleepAfterStopDuration));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        builderSettings.put(BUILDER_SLEEP_AFTER_STOP_DURATION_36X_KEY, Integer.toString(sleepAfterStopDuration));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSuggestedSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        builderSettings.put(BUILDER_SLEEP_AFTER_STOP_DURATION_DEFAULT_KEY, Integer.toString(sleepAfterStopDuration));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setFastStartJoinTimeoutMilliseconds(final int joinTimeoutMilliseconds) {
        builderSettings.put(BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS, Integer.toString(joinTimeoutMilliseconds));

        return this;
    }

    private static URL[] getClassPathUrlsExcludingJavaHome(final String... jarsToExcludeFromClassPath) {
        //TODO: littlegrid#7 Pull this out and add support for wildcards, e.g. *jmx*
        final String pathSeparator = System.getProperty("path.separator");
        final String[] classPathArray = System.getProperty("java.class.path").split(pathSeparator);
        final String javaHome = System.getProperty("java.home");

        final List<URL> classPathUrls = new ArrayList<URL>();

        for (final String partOfClassPath : classPathArray) {
            if (!partOfClassPath.startsWith(javaHome)) {
                boolean includeInClassPath = true;

                if (jarsToExcludeFromClassPath != null) {
                    for (final String jarToExclude : jarsToExcludeFromClassPath) {
                        if (partOfClassPath.endsWith(jarToExclude)) {
                            LOGGER.debug(format("JAR: '%s' specified for exclusion from class path", jarToExclude));

                            includeInClassPath = false;
                        }
                    }
                }

                if (includeInClassPath) {
                    try {
                        classPathUrls.add(new File(partOfClassPath).toURI().toURL());
                    } catch (MalformedURLException e) {
                        throw new IllegalStateException(e);
                    }
                }
            }
        }

        return classPathUrls.toArray(new URL[classPathUrls.size()]);
    }

    /**
     * Returns the system properties that have been configured and will be used for a storage
     * enabled member.
     *
     * @return properties to be applied to system properties.
     */
    public Properties getSystemPropertiesForStorageEnabled() {
        final Properties properties = getSystemPropertiesForTcmpClusterMember();

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.TRUE.toString());

        setPropertyWhenValid(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY);

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

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_CUSTOM_CONFIGURED_ROLE_NAME_KEY);

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

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_EXTEND_PROXY_ROLE_NAME_KEY);

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.TRUE.toString());

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_PORT_KEY),
                Integer.toString(extendPort));

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

        setPropertyWhenValid(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        setPropertyWhenValid(properties, BUILDER_OVERRIDE_CONFIGURATION_KEY);

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.TRUE.toString());

        setPropertyWhenValid(properties, BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY);

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.TRUE.toString());

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_PORT_KEY),
                Integer.toString(extendPort));

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

        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration == null) {
            setPropertyWhenValid(properties, BUILDER_CACHE_CONFIGURATION_KEY);
        } else {
            setPropertyWhenValid(properties, BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        }

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY);

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.FALSE.toString());

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private Properties getSystemPropertiesForTcmpClusterMember() {
        final Properties properties = new Properties();

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_TCMP_ENABLED_KEY),
                Boolean.TRUE.toString());

        setPropertyWhenValid(properties, BUILDER_WKA_ADDRESS_KEY);

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_LOCAL_ADDRESS_KEY),
                getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_ADDRESS_KEY),
                getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));

        setPropertyWhenValid(properties, BUILDER_WKA_PORT_KEY);

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_LOCAL_PORT_KEY),
                getBuilderSettingAsString(BUILDER_WKA_PORT_KEY));

        setPropertyWhenValid(properties, BUILDER_TTL_KEY);
        setPropertyWhenValid(properties, BUILDER_CLUSTER_NAME_KEY);

        setPropertyWhenValid(properties, BUILDER_LOG_DESTINATION_KEY);
        setPropertyWhenValid(properties, BUILDER_LOG_LEVEL_KEY);

        setPropertyWhenValid(properties, BUILDER_FAST_START_JOIN_TIMEOUT_MILLISECONDS);

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

        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration == null) {
            LOGGER.warn("No client cache configuration has been specified for Extend clients");
        } else {
            setPropertyWhenValid(properties, BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        }

        setPropertyWhenValid(properties,
                builderSystemPropertyMapping.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_TCMP_ENABLED_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY);

        setPropertyWhenValid(properties, builderSystemPropertyMapping.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.FALSE.toString());

        setPropertyWhenValid(properties, BUILDER_EXTEND_PORT_KEY);

        setPropertyWhenValid(properties, BUILDER_LOG_DESTINATION_KEY);
        setPropertyWhenValid(properties, BUILDER_LOG_LEVEL_KEY);

        properties.putAll(additionalSystemProperties);

        return properties;
    }

    private void setPropertyWhenValid(final Properties properties,
                                      final String builderSettingKey) {

        final String systemPropertyName = builderSystemPropertyMapping.getProperty(builderSettingKey);

        if (systemPropertyName == null) {
            throw new IllegalStateException(format(
                    "Builder setting key of: '%s' didn't return a mapping to system property name - check "
                            + "mapping of system properties", builderSettingKey));
        }

        setPropertyWhenValid(properties, systemPropertyName, getBuilderSettingAsString(builderSettingKey));
    }

    private void setPropertyWhenValid(final Properties properties,
                                      final String key,
                                      final String value) {

        if (key == null) {
            throw new IllegalArgumentException(format("System property key cannot be null for value of: '%s'", value));
        }

        if (value != null && !value.isEmpty()) {
            properties.setProperty(key, value);
        }
    }
}
