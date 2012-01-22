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
import org.littlegrid.common.PropertiesUtils;

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
    /**
     * Constant for defining standard: tangosol.coherence.
     */
    private static final String TANGOSOL_COHERENCE_DOT = "tangosol.coherence.";

    /**
     * Constant for standard log level system property.
     */
    private static final String COHERENCE_LOG_LEVEL_KEY = TANGOSOL_COHERENCE_DOT + "log.level";

    /**
     * Constant for standard role property.
     */
    private static final String COHERENCE_ROLE_NAME_KEY = TANGOSOL_COHERENCE_DOT + "role";

    /**
     * Constant for standard distributed local storage property.
     */
    private static final String COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY = TANGOSOL_COHERENCE_DOT
            + "distributed.localstorage";

    /**
     * Constant for standard Extend enabled property.
     */
    private static final String COHERENCE_EXTEND_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "extend.enabled";

    /**
     * Constant for standard Extend port property.
     */
    private static final String COHERENCE_EXTEND_PORT_KEY = TANGOSOL_COHERENCE_DOT + "extend.port";

    /**
     * Constant for standard TCMP enabled property.
     */
    private static final String COHERENCE_TCMP_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "tcmp.enabled";

    /**
     * Constant for standard WKA address property.
     */
    private static final String COHERENCE_WKA_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "wka";

    /**
     * Constant for standard WKA port property.
     */
    private static final String COHERENCE_WKA_PORT_KEY = TANGOSOL_COHERENCE_DOT + "wka.port";

    /**
     * Constant for standard local address property.
     */
    private static final String COHERENCE_LOCAL_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "localhost";

    /**
     * Constant for standard local port property.
     */
    private static final String COHERENCE_LOCAL_PORT_KEY = TANGOSOL_COHERENCE_DOT + "localport";

    /**
     * Constant for standard TTL property.
     */
    private static final String COHERENCE_TTL_KEY = TANGOSOL_COHERENCE_DOT + "ttl";

    /**
     * Constant for standard log property.
     */
    private static final String COHERENCE_LOG_KEY = TANGOSOL_COHERENCE_DOT + "log";

    /**
     * Constant for standard cache configuration property.
     */
    private static final String COHERENCE_CACHE_CONFIGURATION_KEY = TANGOSOL_COHERENCE_DOT + "cacheconfig";

    /**
     * Constant for standard override configuration property.
     */
    private static final String COHERENCE_OVERRIDE_KEY = TANGOSOL_COHERENCE_DOT + "override";

    /**
     * Constant for standard cluster configuration property.
     */
    private static final String COHERENCE_CLUSTER_NAME_KEY = TANGOSOL_COHERENCE_DOT + "cluster";

    /**
     * Constant for standard management property.
     */
    private static final String COHERENCE_MANAGEMENT_KEY = TANGOSOL_COHERENCE_DOT + "management";

    /**
     * Constant for standard remote management property.
     */
    private static final String JMX_MANAGEMENT_REMOTE_KEY = "management.remote";

    /**
     * Constant for standard JMX remote management property.
     */
    private static final String JMX_JMXREMOTE_KEY = "com.sun.management.jmxremote";

    private static final String LITTLEGRID_BUILDER_OVERRIDE = "littlegrid.builder.override";

    private static final String BUILDER_STORAGE_ENABLED_COUNT_KEY = "storageEnabledCount";
    private static final String BUILDER_EXTEND_PROXY_COUNT_KEY = "extendProxyCount";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_COUNT_KEY = "storageEnabledExtendProxyCount";

    private static final String BUILDER_NUMBER_OF_THREADS_IN_START_UP_POOL_KEY = "numberOfThreadsInStartUpPool";
    private static final String BUILDER_CLUSTER_MEMBER_INSTANCE_CLASS_NAME_KEY = "clusterMemberInstanceClassName";

    private static final String BUILDER_CACHE_CONFIGURATION_KEY = "cacheConfiguration";
    private static final String BUILDER_CLIENT_CACHE_CONFIGURATION_KEY = "clientCacheConfiguration";
    private static final String BUILDER_OVERRIDE_CONFIGURATION_KEY = "overrideConfiguration";

    private static final String BUILDER_CLUSTER_NAME_KEY = "clusterName";
    private static final String BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY = "storageEnabledRoleName";
    private static final String BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY = "storageDisabledClientRoleName";
    private static final String BUILDER_EXTEND_PROXY_ROLE_NAME_KEY = "extendProxyRoleName";
    private static final String BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY = "storageEnabledExtendProxyRoleName";
    private static final String BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY = "extendClientRoleName";

    private static final String BUILDER_WKA_PORT_KEY = "wkaPort";
    private static final String BUILDER_WKA_ADDRESS_KEY = "wkaAddress";
    private static final String BUILDER_EXTEND_PORT_KEY = "extendPort";
    private static final String BUILDER_TTL_KEY = "ttl";

    private static final String BUILDER_LOG_LEVEL_KEY = "logLevel";

    private static final String BUILDER_DEFAULT_PROPERTIES_FILENAME = "coherence/littlegrid-builder-default.properties";
    private static final String BUILDER_OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-override.properties";

    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(DefaultClusterMemberGroupBuilder.class.getName());

    private Map<String, Object> builderSettings = new HashMap<String, Object>();

    private Properties systemProperties = new Properties();

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
        loadAndProcessProperties();
    }

    /**
     * Returns the current builder settings with their internal builder keys.
     *
     * @return builder settings.
     */
    public Map<String, Object> getBuilderSettings() {
        return builderSettings;
    }

    public Properties getStorageEnabledPropertiesFromBuilderSettings() {
        throw new UnsupportedOperationException();
    }

    public Properties getExtendProxyPropertiesFromBuilderSettings() {
        throw new UnsupportedOperationException();
    }

    public Properties getStorageEnabledExtendProxyPropertiesFromBuilderSettings() {
        throw new UnsupportedOperationException();
    }

    public Properties getExtendClientPropertiesFromBuilderSettings() {
        throw new UnsupportedOperationException();
    }

    private void loadAndProcessProperties() {
        final String overridePropertiesFilename =
                System.getProperty(LITTLEGRID_BUILDER_OVERRIDE, BUILDER_OVERRIDE_PROPERTIES_FILENAME);

        final Properties defaultProperties = PropertiesUtils.loadProperties(BUILDER_DEFAULT_PROPERTIES_FILENAME);
        final Properties overrideProperties = PropertiesUtils.loadProperties(overridePropertiesFilename);

        LOGGER.info(format("Loaded '%s' properties from '%s'", overrideProperties.size(),
                overridePropertiesFilename));

        final Properties propertiesToProcess = new Properties();
        propertiesToProcess.putAll(defaultProperties);
        propertiesToProcess.putAll(overrideProperties);

        BeanUtils.processProperties(this, propertiesToProcess);
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

        if (key != null && value != null && !value.isEmpty()) {
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

    private void preparePropertiesForStorageEnabled() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());

        setSystemPropertyWhenValid(COHERENCE_CACHE_CONFIGURATION_KEY,
                getBuilderSettingAsString(BUILDER_CACHE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_OVERRIDE_KEY,
                getBuilderSettingAsString(BUILDER_OVERRIDE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_ROLE_NAME_KEY,
                getBuilderSettingAsString(BUILDER_STORAGE_ENABLED_ROLE_NAME_KEY));

        setSystemPropertyWhenValid(COHERENCE_LOG_LEVEL_KEY, getBuilderSettingAsString(BUILDER_LOG_LEVEL_KEY));
    }

    private void preparePropertiesForExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());

        setSystemPropertyWhenValid(COHERENCE_CACHE_CONFIGURATION_KEY,
                getBuilderSettingAsString(BUILDER_CACHE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_OVERRIDE_KEY,
                getBuilderSettingAsString(BUILDER_OVERRIDE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_ROLE_NAME_KEY,
                getBuilderSettingAsString(BUILDER_EXTEND_PROXY_ROLE_NAME_KEY));

        setSystemPropertyWhenValid(COHERENCE_LOG_LEVEL_KEY, getBuilderSettingAsString(BUILDER_LOG_LEVEL_KEY));

        setSystemPropertyWhenValid(COHERENCE_EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(COHERENCE_EXTEND_PORT_KEY, getBuilderSettingAsString(BUILDER_EXTEND_PORT_KEY));

        if (extendProxySpecificSystemProperties != null) {
            systemProperties.putAll(extendProxySpecificSystemProperties);
        }
    }

    private void preparePropertiesForStorageEnabledExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(COHERENCE_CACHE_CONFIGURATION_KEY,
                getBuilderSettingAsString(BUILDER_CACHE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_OVERRIDE_KEY,
                getBuilderSettingAsString(BUILDER_OVERRIDE_CONFIGURATION_KEY));

        setSystemPropertyWhenValid(COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());

        setSystemPropertyWhenValid(COHERENCE_LOG_LEVEL_KEY, getBuilderSettingAsString(BUILDER_LOG_LEVEL_KEY));

        setSystemPropertyWhenValid(COHERENCE_ROLE_NAME_KEY,
                getBuilderSettingAsString(BUILDER_STORAGE_ENABLED_PROXY_ROLE_NAME_KEY));

        setSystemPropertyWhenValid(COHERENCE_EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(COHERENCE_EXTEND_PORT_KEY, getBuilderSettingAsString(BUILDER_EXTEND_PORT_KEY));
    }

    private void preparePropertiesForStorageDisabledClient() {
        preparePropertiesForTcmpClusterMember();

        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(COHERENCE_CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());

        setSystemPropertyWhenValid(COHERENCE_ROLE_NAME_KEY,
                getBuilderSettingAsString(BUILDER_STORAGE_DISABLED_CLIENT_ROLE_NAME_KEY));

        setSystemPropertyWhenValid(COHERENCE_EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
    }

    private void preparePropertiesForTcmpClusterMember() {
        setSystemPropertyWhenValid(COHERENCE_TCMP_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(COHERENCE_WKA_ADDRESS_KEY, getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));
        setSystemPropertyWhenValid(COHERENCE_LOCAL_ADDRESS_KEY, getBuilderSettingAsString(BUILDER_WKA_ADDRESS_KEY));
        setSystemPropertyWhenValid(COHERENCE_WKA_PORT_KEY, getBuilderSettingAsString(BUILDER_WKA_PORT_KEY));
        setSystemPropertyWhenValid(COHERENCE_LOCAL_PORT_KEY, getBuilderSettingAsString(BUILDER_WKA_PORT_KEY));
        setSystemPropertyWhenValid(COHERENCE_TTL_KEY, getBuilderSettingAsString(BUILDER_TTL_KEY));
        setSystemPropertyWhenValid(COHERENCE_CLUSTER_NAME_KEY, getBuilderSettingAsString(BUILDER_CLUSTER_NAME_KEY));
    }

    private void preparePropertiesForExtendProxyClient() {
        final String clientCacheConfiguration = getBuilderSettingAsString(BUILDER_CLIENT_CACHE_CONFIGURATION_KEY);

        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(COHERENCE_CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(COHERENCE_DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(COHERENCE_TCMP_ENABLED_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(COHERENCE_ROLE_NAME_KEY,
                getBuilderSettingAsString(BUILDER_EXTEND_CLIENT_ROLE_NAME_KEY));

        setSystemPropertyWhenValid(COHERENCE_EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(COHERENCE_EXTEND_PORT_KEY, getBuilderSettingAsString(BUILDER_EXTEND_PORT_KEY));
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
    public ClusterMemberGroup.Builder setCoherence35xOrOlderSleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoherence35xSleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoherence36xSleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoherence370SleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoherence371SleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCoherenceDefaultSleepAfterStopDuration(final int sleepAfterStopDuration) {
//        throw new UnsupportedOperationException();

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
}
