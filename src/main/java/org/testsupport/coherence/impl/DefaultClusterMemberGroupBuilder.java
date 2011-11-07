package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.utils.BeanUtils;
import org.testsupport.common.utils.LoggerWrapper;
import org.testsupport.common.utils.SystemUtils;

import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import static org.testsupport.coherence.CoherenceSystemPropertyConst.CACHE_CONFIGURATION_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.DISTRIBUTED_LOCAL_STORAGE_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_ADDRESS_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_PORT_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOG_LEVEL_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.OVERRIDE_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.ROLE_NAME_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TTL_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_ADDRESS_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final LoggerWrapper LOGGER = new LoggerWrapper(DefaultClusterMemberGroupBuilder.class.getName());
    private Properties systemProperties = new Properties();
    private int storageEnabledCount;
    private int extendProxyCount;
    private int storageEnabledExtendProxyCount;
    private String cacheConfiguration;
    private String overrideConfiguration;
    private int wkaPort;
    private int localPort = wkaPort;
    private String wkaAddress;
    private String localAddress = wkaAddress;
    private String clusterMemberInstanceClassName;
    private int numberOfThreadsInStartUpPool;
    private int logLevel;
    private String[] jarsToExcludeFromClassPath;

    private static final String DEFAULT_PROPERTIES_FILENAME = "testsupport.coherence.default.properties";
    private static final String OVERRIDE_PROPERTIES_FILENAME = "testsupport.coherence.override.properties";
    private String storageEnabledRoleName;
    private String storageDisabledClientRoleName;


    //TODO: Think about JMX
//            properties.addSystemProperty(MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMXREMOTE_KEY, "");

    public String getStorageEnabledRoleName() {
        return storageEnabledRoleName;
    }

    @Override
    public ClusterMemberGroup.Builder setStorageEnabledRoleName(String storageEnabledRoleName) {
        this.storageEnabledRoleName = storageEnabledRoleName;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(String roleName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setExtendProxyRoleName(String roleName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(String roleName) {
        this.storageDisabledClientRoleName = roleName;

        return this;
    }

    public String getTtl() {
        return ttl;
    }

    public void setTtl(String ttl) {
        this.ttl = ttl;
    }

    private String ttl;

    /*
        Order of settings should be:
            * testsupport.coherence.default.properties
            * testsupport.coherence.project.properties
            * ${ENV.testsupport.coherence}/testsupport.coherence.override.properties
            * -Dtestsupport.coherence.override.properties=nameOfOverridePropertiesFile (resource or file).

        The idea is to be as gently opinionated *but* must also be Developer and CI friendly!
     */
    public DefaultClusterMemberGroupBuilder() {
        loadAndProcessProperties();
    }

    private void loadAndProcessProperties() {
        try {
            Properties defaultProperties = new Properties();
            defaultProperties.load(this.getClass().getClassLoader().getResourceAsStream(DEFAULT_PROPERTIES_FILENAME));

            System.out.println("ADD SUPPORT TO LOAD OVERRIDE PROPERTIES");
//            Properties overrideProperties = new Properties();
//            overrideProperties.load(this.getClass().getClassLoader().getResourceAsStream(OVERRIDE_PROPERTIES_FILENAME));

            Properties properties = new Properties(defaultProperties);
//            properties.putAll(overrideProperties);

            BeanUtils.processProperties(this, properties);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private void setSystemPropertyWhenValid(final String key,
                                            final String value) {

        if (key != null && value != null && !value.isEmpty()) {
            systemProperties.setProperty(key, value);
        }
    }

    @Override
    public ClusterMemberGroup build() {
        ClusterMemberGroup storageEnabledGroup;
        ClusterMemberGroup storageEnabledExtendProxyGroup;
        ClusterMemberGroup extendProxyGroup;

        boolean clientIsExtend = false;

        if (storageEnabledCount == 0 && storageEnabledExtendProxyCount == 0 && extendProxyCount == 0) {
            storageEnabledCount = 1;
        }

        if (storageEnabledExtendProxyCount > 0 || extendProxyCount > 0) {
            clientIsExtend = true;
        }

        prepareProperties();

        if (storageEnabledCount > 0) {
            storageEnabledGroup = new DefaultLocalProcessClusterMemberGroup(storageEnabledCount, systemProperties,
                    null, null, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool).startAll();
        } else {
            throw new UnsupportedOperationException();
        }
////        TODO: SET THE CLUSTER NAME TO THE TIME THE THING WAS STARTED TO HELP TELL IT APART FROM OTHERS RUNNING, PERHAPS IN A DEBUG SESSION.
//        switch (topology) {
//            case STORAGE_ENABLED_ONLY:
//                return createCacheServerGroup(storageEnabledCount, cacheConfiguration, systemProperties,
//                        groupConfig, false);
//
//            case COMPOSITE_STORAGE_ENABLED_PROXY:
//                break;
//
//            case SEPARATE_PROXY_AND_STORAGE_ENABLED:
//                break;
//
//            case EXTEND_PROXY_ONLY:
//                return createExtendProxyServerGroup(storageEnabledCount, cacheConfiguration, systemProperties,
//                        groupConfig, false);
//
//        }

        systemProperties.clear();

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageDisabledClientRoleName);

        if (clientIsExtend) {
            setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        }

        SystemUtils.applyToSystemProperties(systemProperties);

        return storageEnabledGroup;
    }

    private void prepareProperties() {
        setSystemPropertyWhenValid(WKA_ADDRESS_KEY, wkaAddress);
        setSystemPropertyWhenValid(LOCAL_ADDRESS_KEY, localAddress);
        setSystemPropertyWhenValid(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemPropertyWhenValid(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageEnabledRoleName);

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(TTL_KEY, ttl);
        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;

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
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setOverrideConfiguration(final String overrideConfiguration) {
        this.overrideConfiguration = overrideConfiguration;

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
    public ClusterMemberGroup.Builder setSystemProperties(final Properties properties) {
        this.systemProperties = properties;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCount(final int numberOfMembers) {
        this.storageEnabledCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getStorageEnabledCount() {
        return storageEnabledCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        this.storageEnabledExtendProxyCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */

    @Override
    public int getStorageEnabledExtendProxyCount() {
        return storageEnabledExtendProxyCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(final int numberOfMembers) {
        this.extendProxyCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendProxyCount() {
        return extendProxyCount;
    }

    @Override
    public ClusterMemberGroup.Builder setLogLevel(final int logLevel) {
        this.logLevel = logLevel;

        return this;
    }

    @Override
    public int getLogLevel() {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getClusterMemberInstanceClassName() {
        return clusterMemberInstanceClassName;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClassPath(final URL[] classPath) {
        throw new UnsupportedOperationException();
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
        this.wkaAddress = wkaAddress;
        this.localAddress = this.wkaAddress;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWkaAddress() {
        return wkaAddress;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        this.wkaPort = wkaPort;
        this.localPort = this.wkaPort;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return wkaPort;
    }

    /**
     * Convenience method to specifically create a group of cache servers and start them.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createCacheServerGroup(int numberOfMembers,
                                                             String cacheConfiguration,
                                                             Properties properties,
//                                                             ClusterMemberGroupConfig groupConfig,
                                                             boolean startImmediately) {

//        PropertyContainer container = internalCreateCacheServerPropertyContainerWithDefaults();
//        container.addProperties(properties);
//
//        return createGenericClusterMemberGroup(numberOfMembers, cacheConfiguration, container.getProperties(),
//                groupConfig, startImmediately);
        throw new UnsupportedOperationException();
    }

    /**
     * Convenience method to create a group of Extend proxy servers.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createExtendProxyServerGroup(int numberOfMembers,
                                                                   String cacheConfiguration,
//                                                                   Properties properties,
//                                                                   ClusterMemberGroupConfig groupConfig,
                                                                   boolean startImmediately) {

        throw new UnsupportedOperationException();
//        if (numberOfMembers > 1) {
//            throw new UnsupportedOperationException("Currently only one Extend proxy is supported in a group");
//        }
//
//        PropertyContainer container = internalCreateExtendProxyServerPropertyContainerWithDefaults();
//        container.addProperties(properties);
//
//        return createGenericClusterMemberGroup(numberOfMembers, cacheConfiguration, container.getProperties(),
//                groupConfig, startImmediately);
    }

    /**
     * Convenience method to specifically create a composite Extend proxy and cache server.
     *
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
                                                                               Properties properties,
//                                                                               ClusterMemberGroupConfig groupConfig,
                                                                               boolean startImmediately) {

//        PropertyContainer container = new PropertyContainer(properties);
//        container.addProperty(CoherenceSystemPropertyConst.ROLE_NAME_KEY, "LocalProcessCombinedServer");
//        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());
//
//        return createExtendProxyServerGroup(1, cacheConfiguration, container.getProperties(),
//                groupConfig, startImmediately);
        throw new UnsupportedOperationException();
    }

    /**
     * Convenience method to create a generic group of cluster members, typically the more specific create
     * cache server member group and create Extend proxy server group would be used rather than this method.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createGenericClusterMemberGroup(int numberOfMembers,
                                                                      String cacheConfiguration,
                                                                      Properties properties,
//                                                                      ClusterMemberGroupConfig groupConfig,
                                                                      boolean startImmediately) {

        throw new UnsupportedOperationException();
//        if (groupConfig == null) {
//            groupConfig = new ClusterMemberGroupConfig();
//        }
//
//        properties.setProperty(CoherenceSystemPropertyConst.CACHE_CONFIGURATION_KEY, cacheConfiguration);
//
//        groupConfig.setNumberOfClusterMembers(numberOfMembers);
//        ClusterMemberGroup memberGroup = null;
//
//        try {
//            memberGroup = new DefaultLocalProcessClusterMemberGroup(properties, groupConfig);
//
//            if (startImmediately) {
//                memberGroup.startAll();
//            }
//
//            return memberGroup;
//        } catch (Throwable throwable) {
//            LOGGER.severe("Failed to start cluster member group - attempting to shutdown");
////TODO: SORT THIS OUT
////
////            shutdownClusterMemberGroups(memberGroup);
//
//            throw new IllegalStateException(throwable);
    }

//    @Deprecated
//    private static PropertyContainer internalCreateGenericClusterMemberPropertyContainerWithDefaults() {
//        final String host = "127.0.0.1";
//        final String port = System.getProperty(WKA_PORT_KEY, Integer.toString(DEFAULT_WKA_PORT));
//        final String log = System.getProperty(CoherenceSystemPropertyConst.LOG_KEY, CoherenceSystemPropertyConst.DEFAULT_LOG);
//        final String logLevel = System.getProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, Integer.toString(CoherenceSystemPropertyConst.DEFAULT_LOG_LEVEL));
//
//        PropertyContainer container = new PropertyContainer();
//        container.addProperty(CoherenceSystemPropertyConst.TCMP_ENABLED_KEY, Boolean.TRUE.toString());
//        container.addProperty(CoherenceSystemPropertyConst.CLUSTER_KEY, "TestLocalProcessCluster");
//        container.addProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, logLevel);
////        container.addProperty(CoherenceSystemPropertyConst.LOG_KEY, log);
//        container.addProperty(CoherenceSystemPropertyConst.WKA_ADDRESS_KEY, host);
//        container.addProperty(WKA_PORT_KEY, port);
//        container.addProperty(CoherenceSystemPropertyConst.LOCAL_ADDRESS_KEY, host);
//        container.addProperty(CoherenceSystemPropertyConst.LOCAL_PORT_KEY, port);
//        container.addProperty(CoherenceSystemPropertyConst.TTL_KEY, "0");
////        container.addProperty(MANAGEMENT_KEY, "all");
////        container.addProperty(MANAGEMENT_REMOTE_KEY, Boolean.TRUE.toString());
////        container.addProperty(JMXREMOTE_KEY, "");
//
//        return container;
//    }

    @Override
    public int getNumberOfThreadsInStartUpPool() {
        return numberOfThreadsInStartUpPool;
    }

    @Override
    public ClusterMemberGroup.Builder setBuilder(Properties properties) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;

        return this;
    }

    @Deprecated
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final String numberOfThreadsInStartUpPool) {
        setNumberOfThreadsInStartUpPool(Integer.parseInt(numberOfThreadsInStartUpPool));

        return this;
    }
}
