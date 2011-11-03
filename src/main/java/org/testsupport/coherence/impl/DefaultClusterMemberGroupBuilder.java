package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.CoherenceSystemPropertyConst;
import org.testsupport.common.lang.PropertyContainer;

import java.net.URL;
import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.testsupport.coherence.ClusterMemberGroup.Builder.Topology.STORAGE_ENABLED_ONLY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.DEFAULT_WKA_PORT;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_PORT_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

/**
 */
public class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupBuilder.class.getName());
    private int memberCount = 1;
    private String cacheConfiguration;
    private Properties systemProperties = new Properties();
    private Topology topology = STORAGE_ENABLED_ONLY;
    private int wkaPort = DEFAULT_WKA_PORT;
    private int localPort = wkaPort;
    private ClusterMemberGroupConfig groupConfig = new ClusterMemberGroupConfig();

    public DefaultClusterMemberGroupBuilder() {
    }

    @Override
    public ClusterMemberGroup build() {
        systemProperties.setProperty(WKA_PORT_KEY, Integer.toString(wkaPort));
        systemProperties.setProperty(LOCAL_PORT_KEY, Integer.toString(localPort));

//        TODO: SET THE CLUSTER NAME TO THE TIME THE THING WAS STARTED TO HELP TELL IT APART FROM OTHERS RUNNING, PERHAPS IN A DEBUG SESSSION.
        switch (topology) {
            case STORAGE_ENABLED_ONLY:
                return createCacheServerGroup(memberCount, cacheConfiguration, systemProperties,
                        groupConfig, false);

            case COMPOSITE_STORAGE_ENABLED_PROXY:
                break;

            case SEPARATE_PROXY_AND_STORAGE_ENABLED:
                break;

            case EXTEND_PROXY_ONLY:
                return createExtendProxyServerGroup(memberCount, cacheConfiguration, systemProperties,
                        groupConfig, false);

        }

        String message = format("Cannot build of type '%s' as it is unrecognised", topology);
        LOGGER.severe(message);
        throw new IllegalArgumentException(message);
    }

    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setClientCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setSystemProperties(final Properties properties) {
        this.systemProperties = properties;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setTopology(final Topology topology) {
        this.topology = topology;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setNumberOfMembers(final int numberOfMembers) {
        this.memberCount = numberOfMembers;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setClusterMemberClassName(final String clusterMemberClassName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setClassPath(final URL[] classPath) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        groupConfig.setJarsToExcludeFromClassPathUrls(jarsToExcludeFromClassPath);

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        this.wkaPort = wkaPort;
        this.localPort = wkaPort;

        return this;
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
    public static ClusterMemberGroup createCacheServerGroup(int numberOfMembers,
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
            memberGroup = new DefaultLocalProcessClusterMemberGroupImpl(container, groupConfig);

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
        container.addProperty(CoherenceSystemPropertyConst.WKA_KEY, host);
        container.addProperty(WKA_PORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.LOCALHOST_KEY, host);
        container.addProperty(CoherenceSystemPropertyConst.LOCAL_PORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.TTL_KEY, "0");
//        container.addProperty(MANAGEMENT_KEY, "all");
//        container.addProperty(MANAGEMENT_REMOTE_KEY, Boolean.TRUE.toString());
//        container.addProperty(JMXREMOTE_KEY, "");

        return container;
    }
}
