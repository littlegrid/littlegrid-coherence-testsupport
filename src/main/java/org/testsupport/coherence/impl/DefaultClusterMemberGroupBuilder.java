package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.CoherenceSystemPropertyConst;
import org.testsupport.common.lang.PropertyContainer;

import java.net.URL;
import java.util.Properties;
import java.util.logging.Logger;

import static org.testsupport.coherence.CoherenceSystemPropertyConst.DEFAULT_WKA_PORT;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_HOST_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_PORT_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.OVERRIDE_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TTL_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_HOST_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());
    private int numberOfStorageEnabledMembers = 1; //TODO: remove this initial value
    private String cacheConfiguration;
    private String overrideConfiguration;
    private Properties systemProperties = new Properties();
    private int wkaPort = DEFAULT_WKA_PORT; //TODO: remove this initial value
    private int localPort = wkaPort;
    private String wkaAddress = "127.0.0.1";
    private String localAddress = wkaAddress;

    private ClusterMemberGroupConfig groupConfig = new ClusterMemberGroupConfig();

    /*
        Order of settings should be:
            * testsupport.coherence.default.properties
            * testsupport.coherence.project.properties
            * ${ENV.testsupport.coherence}/testsupport.coherence.override.properties
            * -Dtestsupport.coherence.override.properties=nameOfOverridePropertiesFile (resource or file).

        The idea is to be as gently opinionated *but* must also be Developer and CI friendly!
     */


    public DefaultClusterMemberGroupBuilder() {
    }

    private void setSystemProperty(final String key,
                                   final String value) {

        if (key != null && value != null) {
            systemProperties.setProperty(key, value);
        }
    }

    @Override
    public ClusterMemberGroup build() {
        setSystemProperty(WKA_HOST_KEY, wkaAddress);
        setSystemProperty(LOCAL_HOST_KEY, localAddress);
        setSystemProperty(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemProperty(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemProperty(OVERRIDE_KEY, overrideConfiguration);
        setSystemProperty(TTL_KEY, "0");

        if (numberOfStorageEnabledMembers > 0) {
            return createCacheServerGroup(numberOfStorageEnabledMembers, cacheConfiguration, systemProperties,
                    groupConfig, false);
        } else {
            throw new UnsupportedOperationException();
        }
////        TODO: SET THE CLUSTER NAME TO THE TIME THE THING WAS STARTED TO HELP TELL IT APART FROM OTHERS RUNNING, PERHAPS IN A DEBUG SESSSION.
//        switch (topology) {
//            case STORAGE_ENABLED_ONLY:
//                return createCacheServerGroup(numberOfStorageEnabledMembers, cacheConfiguration, systemProperties,
//                        groupConfig, false);
//
//            case COMPOSITE_STORAGE_ENABLED_PROXY:
//                break;
//
//            case SEPARATE_PROXY_AND_STORAGE_ENABLED:
//                break;
//
//            case EXTEND_PROXY_ONLY:
//                return createExtendProxyServerGroup(numberOfStorageEnabledMembers, cacheConfiguration, systemProperties,
//                        groupConfig, false);
//
//        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setExtendProxyCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCacheConfiguration(final String cacheConfiguration) {
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
        this.numberOfStorageEnabledMembers = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterMemberClassName(final String clusterMemberClassName) {
        throw new UnsupportedOperationException();
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
        groupConfig.setJarsToExcludeFromClassPathUrls(jarsToExcludeFromClassPath);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        this.wkaPort = wkaPort;
        this.localPort = wkaPort;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(int numberOfMembers) {
        throw new UnsupportedOperationException();
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
                                                             ClusterMemberGroupConfig groupConfig,
                                                             boolean startImmediately) {

        PropertyContainer container = internalCreateCacheServerPropertyContainerWithDefaults();
        container.addProperties(properties);

        return createGenericClusterMemberGroup(numberOfMembers, cacheConfiguration, container.getProperties(),
                groupConfig, startImmediately);
    }

    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createExtendProxyServerGroup(String cacheConfiguration) {

        return createExtendProxyServerGroup(1, cacheConfiguration);
    }


    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createExtendProxyServerGroup(int numberOfMembers,
                                                                   String cacheConfiguration) {

        return createExtendProxyServerGroup(numberOfMembers, cacheConfiguration, null);
    }

    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createExtendProxyServerGroup(int numberOfMembers,
                                                                   String cacheConfiguration,
                                                                   Properties properties) {

        return createExtendProxyServerGroup(numberOfMembers, cacheConfiguration, properties, null);
    }

    /**
     * Convenience method to specifically create a group of Extend proxy servers and start them.
     *
     * @param numberOfMembers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createExtendProxyServerGroup(int numberOfMembers,
                                                                   String cacheConfiguration,
                                                                   Properties properties,
                                                                   ClusterMemberGroupConfig groupConfig) {

        return createExtendProxyServerGroup(numberOfMembers, cacheConfiguration, properties, groupConfig, true);
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
                                                                   Properties properties,
                                                                   ClusterMemberGroupConfig groupConfig,
                                                                   boolean startImmediately) {

        if (numberOfMembers > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is supported in a group");
        }

        PropertyContainer container = internalCreateExtendProxyServerPropertyContainerWithDefaults();
        container.addProperties(properties);

        return createGenericClusterMemberGroup(numberOfMembers, cacheConfiguration, container.getProperties(),
                groupConfig, startImmediately);
    }

    /**
     * Convenience method to create a composite Extend proxy and cache server.
     *
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration) {
        return createSingleCompositeProxyAndCacheServer(cacheConfiguration, null);
    }

    /**
     * Convenience method to create a composite Extend proxy and cache server.
     *
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
                                                                               Properties properties) {

        return createSingleCompositeProxyAndCacheServer(cacheConfiguration, properties, null);
    }

    /**
     * Convenience method to create a composite Extend proxy and cache server.
     *
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @return member group.
     */
    @Deprecated
    private static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
                                                                               Properties properties,
                                                                               ClusterMemberGroupConfig groupConfig) {

        return createSingleCompositeProxyAndCacheServer(cacheConfiguration, properties, groupConfig, true);
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
                                                                               ClusterMemberGroupConfig groupConfig,
                                                                               boolean startImmediately) {

        PropertyContainer container = new PropertyContainer(properties);
        container.addProperty(CoherenceSystemPropertyConst.ROLE_KEY, "LocalProcessCombinedServer");
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.TRUE.toString());

        return createExtendProxyServerGroup(1, cacheConfiguration, container.getProperties(),
                groupConfig, startImmediately);
    }

    @Deprecated
    private static ClusterMemberGroup createSingleExtendProxyWithCacheServerGroup(int numberOfCacheServers,
                                                                                  String cacheConfiguration,
                                                                                  Properties properties,
                                                                                  ClusterMemberGroupConfig groupConfig,
                                                                                  boolean startImmediately) {

        throw new UnsupportedOperationException("Not implemented yet");
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
                                                                      ClusterMemberGroupConfig groupConfig,
                                                                      boolean startImmediately) {

        if (groupConfig == null) {
            groupConfig = new ClusterMemberGroupConfig();
        }

        PropertyContainer container = new PropertyContainer(properties);
        container.addProperty(CoherenceSystemPropertyConst.CACHECONFIG_KEY, cacheConfiguration);

        groupConfig.setNumberOfClusterMembers(numberOfMembers);
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = new DefaultLocalProcessClusterMemberGroup(container, groupConfig);

            if (startImmediately) {
                memberGroup.startAll();
            }

            return memberGroup;
        } catch (Throwable throwable) {
            LOGGER.severe("Failed to start cluster member group - attempting to shutdown");
//TODO: SORT THIS OUT
//
//            shutdownClusterMemberGroups(memberGroup);

            throw new IllegalStateException(throwable);
        }
    }

    /**
     * Creates properties for local in-process cache server.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createCacheServerPropertiesWithDefaults() {
        return internalCreateCacheServerPropertyContainerWithDefaults().getProperties();
    }

    private static PropertyContainer internalCreateCacheServerPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(
                internalCreateGenericClusterMemberPropertyContainerWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.ROLE_KEY, "LocalProcessCacheServer");
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.TRUE.toString());
        container.addProperty(CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY, Boolean.FALSE.toString());

        return container;
    }

    /**
     * Creates properties for local in-process Extend proxy server.
     *
     * @return properties.
     */
    @Deprecated
    private static Properties createExtendProxyServerPropertiesWithDefaults() {
        return internalCreateCacheServerPropertyContainerWithDefaults().getProperties();
    }

    @Deprecated
    private static PropertyContainer internalCreateExtendProxyServerPropertyContainerWithDefaults() {
        PropertyContainer container =
                new PropertyContainer(internalCreateGenericClusterMemberPropertyContainerWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.ROLE_KEY, "LocalProcessExtendProxyServer");
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());
        container.addProperty(CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY, Boolean.TRUE.toString());

        return container;
    }

    /**
     * Creates properties for local in-process cluster members.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createGenericClusterMemberPropertiesWithDefaults() {
        return internalCreateGenericClusterMemberPropertyContainerWithDefaults().getProperties();
    }

    @Deprecated
    private static PropertyContainer internalCreateGenericClusterMemberPropertyContainerWithDefaults() {
        final String host = "127.0.0.1";
        final String port = System.getProperty(WKA_PORT_KEY, Integer.toString(DEFAULT_WKA_PORT));
        final String log = System.getProperty(CoherenceSystemPropertyConst.LOG_KEY, CoherenceSystemPropertyConst.DEFAULT_LOG);
        final String logLevel = System.getProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, Integer.toString(CoherenceSystemPropertyConst.DEFAULT_LOG_LEVEL));

        PropertyContainer container = new PropertyContainer();
        container.addProperty(CoherenceSystemPropertyConst.TCMP_ENABLED_KEY, Boolean.TRUE.toString());
        container.addProperty(CoherenceSystemPropertyConst.CLUSTER_KEY, "TestLocalProcessCluster");
        container.addProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, logLevel);
//        container.addProperty(CoherenceSystemPropertyConst.LOG_KEY, log);
        container.addProperty(CoherenceSystemPropertyConst.WKA_HOST_KEY, host);
        container.addProperty(WKA_PORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.LOCAL_HOST_KEY, host);
        container.addProperty(CoherenceSystemPropertyConst.LOCAL_PORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.TTL_KEY, "0");
//        container.addProperty(MANAGEMENT_KEY, "all");
//        container.addProperty(MANAGEMENT_REMOTE_KEY, Boolean.TRUE.toString());
//        container.addProperty(JMXREMOTE_KEY, "");

        return container;
    }
}
