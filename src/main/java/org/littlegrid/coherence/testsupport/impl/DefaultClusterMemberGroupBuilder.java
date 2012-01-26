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
    private static final String TANGOSOL_COHERENCE_DOT = "tangosol.coherence.";

    private static final String BUILDER_STORAGE_ENABLED_COUNT_KEY = "StorageEnabledCount";
    private static final String BUILDER_EXTEND_PROXY_COUNT_KEY = "ExtendProxyCount";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY = "StorageEnabledExtendProxyCount";

    private static final String BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "NumberOfThreadsInStartUpPool";
    private static final String BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "ClusterMemberInstanceClassName";

    private static final String BUILDER_CACHE_CONFIGURATION_KEY = "CacheConfiguration";
    private static final String BUILDER_CLIENT_CACHE_CONFIGURATION_KEY = "ClientCacheConfiguration";
    private static final String BUILDER_OVERRIDE_CONFIGURATION_KEY = "OverrideConfiguration";

    private static final String BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY = "DistributedLocalStorage";
    private static final String BUILDER_TCMP_ENABLED_KEY = "TcmpEnabled";

    private static final String BUILDER_CLUSTER_NAME_KEY = "ClusterName";
    private static final String BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY = "StorageEnabledRoleName";
    private static final String BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "StorageDisabledClientRoleName";
    private static final String BUILDER_EXTEND_PROXY_ROLE_NAME_KEY = "ExtendProxyRoleName";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "StorageEnabledExtendProxyRoleName";
    private static final String BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY = "ExtendClientRoleName";

    private static final String BUILDER_WKA_PORT_KEY = "WkaPort";
    private static final String BUILDER_LOCAL_ADDRESS_KEY = "LocalAddress";
    private static final String BUILDER_LOCAL_PORT_KEY = "LocalPort";
    private static final String BUILDER_WKA_ADDRESS_KEY = "WkaAddress";
    private static final String BUILDER_EXTEND_PORT_KEY = "ExtendPort";
    private static final String BUILDER_TTL_KEY = "Ttl";

    private static final String BUILDER_EXTEND_ENABLED_KEY = "ExtendEnabled";

    private static final String BUILDER_LOG_LEVEL_KEY = "LogLevel";

    private static final String SLEEP_AFTER_STOP_DURATION_PRE35X = "sleepAfterStopDurationPre35x";
    private static final String SLEEP_AFTER_STOP_DURATION_35X = "version35xSleepAfterStopDuration";
    private static final String SLEEP_AFTER_STOP_DURATION_36X = "version36xSleepAfterStopDuration";
    private static final String SLEEP_AFTER_STOP_DURATION_370 = "version370SleepAfterStopDuration";
    private static final String SLEEP_AFTER_STOP_DURATION_DEFAULT = "defaultSleepAfterStopDuration";

    private static final String BUILDER_DEFAULT_PROPERTIES_FILENAME = "coherence/littlegrid-builder-default.properties";
    private static final String BUILDER_OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";
    private static final String LITTLEGRID_BUILDER_OVERRIDE = "littlegrid.builder.override";

    private static final String BUILDER_DEFAULT_MAPPING_PROPERTIES_FILENAME =
            "coherence/littlegrid-builder-default-mapping.properties";


    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(DefaultClusterMemberGroupBuilder.class.getName());

    private Map<String, Object> builderSettings = new HashMap<String, Object>();

    private Properties systemProperties = new Properties();
    private Properties builderMappingSettings = new Properties();

    private String[] jarsToExcludeFromClassPath;
    private URL[] classPathUrls;

    private Properties extendProxySpecificSystemProperties;

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

    private void loadBuilderMappingSettings() {
        builderMappingSettings = PropertiesUtils.loadProperties(BUILDER_DEFAULT_MAPPING_PROPERTIES_FILENAME);
    }

    /**
     * Returns the current builder settings with their internal builder keys.
     *
     * @return builder settings.
     */
    public Map<String, Object> getBuilderSettings() {
        return builderSettings;
    }

    public Properties getStorageEnabledSystemPropertiesToApply() {
        final Properties properties = new Properties();

        properties.setProperty(builderMappingSettings.getProperty(BUILDER_TCMP_ENABLED_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_WKA_ADDRESS_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_LOCAL_ADDRESS_KEY),
                getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));

        setSystemPropertyWhenValid(BUILDER_WKA_PORT_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_LOCAL_PORT_KEY),
                getBuilderSettingAsString(BUILDER_WKA_PORT_KEY));

        setSystemPropertyWhenValid(BUILDER_TTL_KEY);
        setSystemPropertyWhenValid(BUILDER_CLUSTER_NAME_KEY);
        throw new UnsupportedOperationException();
//        return properties;
    }

    public Properties getStorageDisabledSystemPropertiesToApply() {
        throw new UnsupportedOperationException();
    }

    public Properties getExtendProxySystemPropertiesToApply() {
        throw new UnsupportedOperationException();
    }

    public Properties getStorageEnabledExtendProxySystemPropertiesToApply() {
        throw new UnsupportedOperationException();
    }

    public Properties getExtendClientSystemPropertiesToApply() {
        throw new UnsupportedOperationException();
    }

    private void loadAndProcessBuilderSettings() {
        final String overridePropertiesFilename =
                System.getProperty(LITTLEGRID_BUILDER_OVERRIDE, BUILDER_OVERRIDE_PROPERTIES_FILENAME);

        final Properties properties = PropertiesUtils.loadProperties(
                BUILDER_DEFAULT_PROPERTIES_FILENAME, overridePropertiesFilename);

        BeanUtils.processProperties(this, properties);
    }

    private int getBuilderSettingAsInt(final String builderKey) {
        return (Integer) builderSettings.get(builderKey);
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

    private void setSystemPropertyWhenValid(final String key,
                                            final String value) {

        if (key == null) {
            throw new IllegalArgumentException(format("System property key cannot be null for value of: '%s'", value));
        }

        if (value != null && !value.isEmpty()) {
            systemProperties.setProperty(key, value);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup build() {
        int storageEnabledCount = getBuilderSettingAsInt(BUILDER_STORAGE_ENABLED_COUNT_KEY);
        final int extendProxyCount = getBuilderSettingAsInt(BUILDER_EXTEND_PROXY_COUNT_KEY);
        final int storageEnabledExtendProxyCount = getBuilderSettingAsInt(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY);

        final int numberOfThreadsInStartUpPool = getBuilderSettingAsInt(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY);
        final String clusterMemberInstanceClassName =
                getBuilderSettingAsString(BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY);

        //TODO: littlegrid#6 Tidy this up
        // on exception output: class path, tangosol system properties, all system properties and message
        // to suggest checking for another running cluster
        final DefaultClusterMemberGroup containerGroup = new DefaultClusterMemberGroup();

        if (storageEnabledCount == 0 && storageEnabledExtendProxyCount == 0 && extendProxyCount == 0) {
            storageEnabledCount = 1;
        }

        if (classPathUrls == null) {
            LOGGER.debug("Cluster member group config class path URLs null, setting to current (minus Java home)");

            this.classPathUrls = getClassPathUrlsExcludingJavaHome(jarsToExcludeFromClassPath);
        }

        if (storageEnabledCount > 0) {
            preparePropertiesForStorageEnabled();

            final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(storageEnabledCount,
                    systemProperties, classPathUrls, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge(memberGroup);
        }

        if (extendProxyCount == 1) {
            preparePropertiesForExtendProxy();

            final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(extendProxyCount,
                    systemProperties, classPathUrls, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge(memberGroup);
        } else if (extendProxyCount > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is currently supported");
        }

        if (storageEnabledExtendProxyCount == 1) {
            preparePropertiesForStorageEnabledExtendProxy();

            final ClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(storageEnabledExtendProxyCount,
                    systemProperties, classPathUrls, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge(memberGroup);
        } else if (storageEnabledExtendProxyCount > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is currently supported");
        }

        systemProperties.clear();

        if (storageEnabledExtendProxyCount > 0 || extendProxyCount > 0) {
            preparePropertiesForExtendProxyClient();
        } else {
            preparePropertiesForStorageDisabledClient();
        }

        SystemUtils.applyToSystemProperties(systemProperties);
        LOGGER.info(format("Coherence system properties for client: %s",
                SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));

        return containerGroup;
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
        systemProperties.putAll(properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setAdditionalSystemProperties(String propertiesFilenames) {
        setAdditionalSystemProperties(PropertiesUtils.loadProperties(propertiesFilenames));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxySpecificSystemProperties(final Properties properties) {
        this.extendProxySpecificSystemProperties = properties;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(final int numberOfMembers) {
        builderSettings.put(BUILDER_EXTEND_PROXY_COUNT_KEY, numberOfMembers);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setLogDestination(final String logDestination) {
        throw new UnsupportedOperationException();
    }

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
        builderSettings.put(BUILDER_LOG_LEVEL_KEY, logLevel);

        return this;
    }

    /**
     * Sets the storage enabled member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageEnabledRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * Sets the storage enabled Extend proxy member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * Sets the Extend proxy member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setExtendProxyRoleName(final String roleName) {
        builderSettings.put(BUILDER_EXTEND_PROXY_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * Sets the storage disabled member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(final String roleName) {
        builderSettings.put(BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY, roleName);

        return this;
    }

    /**
     * Sets the Extend client's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
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
        builderSettings.put(BUILDER_WKA_PORT_KEY, wkaPort);

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
        builderSettings.put(BUILDER_EXTEND_PORT_KEY, extendPort);

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
    public ClusterMemberGroup.Builder setSleepAfterStopDurationPre35x(final int sleepAfterStopDuration) {
        builderSettings.put(SLEEP_AFTER_STOP_DURATION_PRE35X, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSleepAfterStopDuration35x(final int sleepAfterStopDuration) {
        builderSettings.put(SLEEP_AFTER_STOP_DURATION_35X, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSleepAfterStopDuration36x(final int sleepAfterStopDuration) {
        builderSettings.put(SLEEP_AFTER_STOP_DURATION_36X, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSleepAfterStopDuration370(final int sleepAfterStopDuration) {
        builderSettings.put(SLEEP_AFTER_STOP_DURATION_370, sleepAfterStopDuration);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSleepAfterStopDurationDefault(final int sleepAfterStopDuration) {
        builderSettings.put(SLEEP_AFTER_STOP_DURATION_DEFAULT, sleepAfterStopDuration);

        return this;
    }

    /**
     * Sets the TTL.
     *
     * @param ttl TTL.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setTtl(final int ttl) {
        builderSettings.put(BUILDER_TTL_KEY, ttl);

        return this;
    }

    /**
     * Sets the number of threads to handle starting up the members within a cluster member group.
     *
     * @param numberOfThreadsInStartUpPool Number of threads available to start-up members.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        builderSettings.put(BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY, numberOfThreadsInStartUpPool);

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

    private void preparePropertiesForStorageEnabled() {

        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_CACHE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY);
        setSystemPropertyWhenValid(BUILDER_LOG_LEVEL_KEY);
    }

    @Deprecated
    private void setSystemPropertyWhenValid(final String builderSettingKey) {
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(builderSettingKey),
                getBuilderSettingAsString(builderSettingKey));
    }

    private void setSystemPropertyWhenValid(final Properties properties,
                                            final String builderSettingKey) {
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(builderSettingKey),
                getBuilderSettingAsString(builderSettingKey));
    }

    private void preparePropertiesForExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setSystemPropertyWhenValid(BUILDER_CACHE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(BUILDER_EXTEND_PROXY_ROLE_NAME_KEY);
        setSystemPropertyWhenValid(BUILDER_LOG_LEVEL_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_EXTEND_PORT_KEY);

        if (extendProxySpecificSystemProperties != null) {
            systemProperties.putAll(extendProxySpecificSystemProperties);
        }
    }

    private void preparePropertiesForStorageEnabledExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(BUILDER_CACHE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(BUILDER_OVERRIDE_CONFIGURATION_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_LOG_LEVEL_KEY);
        setSystemPropertyWhenValid(BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_EXTEND_PORT_KEY);
    }

    private void preparePropertiesForStorageDisabledClient() {
        preparePropertiesForTcmpClusterMember();

        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration == null) {
            setSystemPropertyWhenValid(BUILDER_CACHE_CONFIGURATION_KEY);
        } else {
            setSystemPropertyWhenValid(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        }

        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setSystemPropertyWhenValid(BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.FALSE.toString());
    }

    private void preparePropertiesForTcmpClusterMember() {
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_TCMP_ENABLED_KEY),
                Boolean.TRUE.toString());

        setSystemPropertyWhenValid(BUILDER_WKA_ADDRESS_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_LOCAL_ADDRESS_KEY),
                getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));

        setSystemPropertyWhenValid(BUILDER_WKA_PORT_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_LOCAL_PORT_KEY),
                getBuilderSettingAsString(BUILDER_WKA_PORT_KEY));

        setSystemPropertyWhenValid(BUILDER_TTL_KEY);
        setSystemPropertyWhenValid(BUILDER_CLUSTER_NAME_KEY);
    }

    private void preparePropertiesForExtendProxyClient() {
        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration == null) {
            LOGGER.warn("No client cache configuration has been specified for Extend clients");
        } else {
            setSystemPropertyWhenValid(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);
        }

        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_DISTRIBUTED_LOCAL_STORAGE_KEY),
                Boolean.FALSE.toString());

        setSystemPropertyWhenValid(BUILDER_TCMP_ENABLED_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY);
        setSystemPropertyWhenValid(builderMappingSettings.getProperty(BUILDER_EXTEND_ENABLED_KEY),
                Boolean.FALSE.toString());

        setSystemPropertyWhenValid(BUILDER_EXTEND_PORT_KEY);
    }
}
