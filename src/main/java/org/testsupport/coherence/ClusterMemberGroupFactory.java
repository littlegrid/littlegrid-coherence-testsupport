package org.testsupport.coherence;

import org.testsupport.coherence.support.impl.ClusterMemberGroupConfig;
import org.testsupport.coherence.support.impl.DefaultClusterMemberGroupBuilder;
import org.testsupport.coherence.support.impl.DefaultLocalProcessClusterMemberGroupImpl;
import org.testsupport.coherence.support.impl.PropertyContainer;
import com.tangosol.net.CacheFactory;
import org.apache.log4j.Logger;

import java.util.Properties;

/**
 * Cluster member group factory.
 */
public class ClusterMemberGroupFactory {
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP = 3;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN = 1;

    private static Logger LOGGER = Logger.getLogger(ClusterMemberGroupFactory.class);

    /**
     * Private constructor to prevent creation.
     */
    private ClusterMemberGroupFactory() {
    }

    public static ClusterMemberGroup.Builder newBuilder() {
        return new DefaultClusterMemberGroupBuilder();
    }

    public static int getSecondsToSleepAfterPerformingShutdown() {
        return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN;
    }

    public static int getSecondsToSleepAfterPerformingStop() {
        return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP;
    }

    //TODO: Think about JMX
//            properties.addSystemProperty(MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMXREMOTE_KEY, "");

    /**
     * Convenience method to create a group of cache servers and start them.
     *
     * @param numberOfServers Number of servers.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createCacheServerGroup(int numberOfServers) {
        return createCacheServerGroup(numberOfServers, null);
    }

    /**
     * Convenience method to create a group of cache servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createCacheServerGroup(int numberOfServers,
                                                            String cacheConfiguration) {

        return createCacheServerGroup(numberOfServers, cacheConfiguration, null);
    }

    /**
     * Convenience method to create a group of cache servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createCacheServerGroup(int numberOfServers,
                                                            String cacheConfiguration,
                                                            Properties properties) {

        return createCacheServerGroup(numberOfServers, cacheConfiguration, properties, null);
    }

    /**
     * Convenience method to create a group of cache servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createCacheServerGroup(int numberOfServers,
                                                            String cacheConfiguration,
                                                            Properties properties,
                                                            ClusterMemberGroupConfig groupConfig) {

        return createCacheServerGroup(numberOfServers, cacheConfiguration, properties, groupConfig, true);
    }

    /**
     * Convenience method to specifically create a group of cache servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createCacheServerGroup(int numberOfServers,
                                                            String cacheConfiguration,
                                                            Properties properties,
                                                            ClusterMemberGroupConfig groupConfig,
                                                            boolean startImmediately) {

        PropertyContainer container = internalCreateCacheServerPropertyContainerWithDefaults();
        container.addProperties(properties);

        return createGenericClusterMemberGroup(numberOfServers, cacheConfiguration, container.getProperties(),
                groupConfig, startImmediately);
    }

    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createExtendProxyServerGroup(String cacheConfiguration) {

        return createExtendProxyServerGroup(1, cacheConfiguration);
    }


    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createExtendProxyServerGroup(int numberOfServers,
                                                                  String cacheConfiguration) {

        return createExtendProxyServerGroup(numberOfServers, cacheConfiguration, null);
    }

    /**
     * Convenience method to create a group of Extend proxy servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createExtendProxyServerGroup(int numberOfServers,
                                                                  String cacheConfiguration,
                                                                  Properties properties) {

        return createExtendProxyServerGroup(numberOfServers, cacheConfiguration, properties, null);
    }

    /**
      * Convenience method to specifically create a group of Extend proxy servers and start them.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createExtendProxyServerGroup(int numberOfServers,
                                                                  String cacheConfiguration,
                                                                  Properties properties,
                                                                  ClusterMemberGroupConfig groupConfig) {

        return createExtendProxyServerGroup(numberOfServers, cacheConfiguration, properties, groupConfig, true);
    }

    /**
     * Convenience method to create a group of Extend proxy servers.
     *
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createExtendProxyServerGroup(int numberOfServers,
                                                                  String cacheConfiguration,
                                                                  Properties properties,
                                                                  ClusterMemberGroupConfig groupConfig,
                                                                  boolean startImmediately) {

        if (numberOfServers > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is supported in a group");
        }

        PropertyContainer container = internalCreateExtendProxyServerPropertyContainerWithDefaults();
        container.addProperties(properties);

        return createGenericClusterMemberGroup(numberOfServers, cacheConfiguration, container.getProperties(),
                groupConfig, startImmediately);
    }

    /**
     * Convenience method to create a composite Extend proxy and cache server.
     *
     * @param cacheConfiguration cache configuration.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration) {
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
    public static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
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
    public static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
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
    public static ClusterMemberGroup createSingleCompositeProxyAndCacheServer(String cacheConfiguration,
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
    public static ClusterMemberGroup createSingleExtendProxyWithCacheServerGroup(int numberOfCacheServers,
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
     * @param numberOfServers    Number of servers.
     * @param cacheConfiguration cache configuration.
     * @param properties         Properties.
     * @param groupConfig        Cluster member group configuration.
     * @param startImmediately   Indicates whether the group should be started immediately after creating.
     * @return member group.
     */
    @Deprecated
    public static ClusterMemberGroup createGenericClusterMemberGroup(int numberOfServers,
                                                                     String cacheConfiguration,
                                                                     Properties properties,
                                                                     ClusterMemberGroupConfig groupConfig,
                                                                     boolean startImmediately) {

        if (groupConfig == null) {
            groupConfig = new ClusterMemberGroupConfig();
        }

        PropertyContainer container = new PropertyContainer(properties);
        container.addProperty(CoherenceSystemPropertyConst.CACHECONFIG_KEY, cacheConfiguration);

        groupConfig.setNumberOfClusterMembers(numberOfServers);
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = new DefaultLocalProcessClusterMemberGroupImpl(container, groupConfig);

            if (startImmediately) {
                memberGroup.startAll();
            }

            return memberGroup;
        } catch (Throwable throwable) {
            LOGGER.error("Failed to start cluster member group - attempting to shutdown");

            shutdownClusterMemberGroups(memberGroup);

            throw new IllegalStateException(throwable);
        }
    }

    /**
     * Shutdown cluster member groups.
     *
     * @param clusterMemberGroups Member groups.
     */
    public static void shutdownClusterMemberGroups(ClusterMemberGroup... clusterMemberGroups) {
        boolean exceptionOccurredDuringShutdown = false;

        for (ClusterMemberGroup clusterMemberGroup : clusterMemberGroups) {
            try {
                if (clusterMemberGroup != null) {
                    clusterMemberGroup.shutdownAll();
                }
            } catch (Exception e) {
                exceptionOccurredDuringShutdown = true;

                LOGGER.error("Could not shutdown cluster member group: " + e);
                // Ignore and allow looping to try and shutdown any other cluster member groups if running
            }
        }

        if (exceptionOccurredDuringShutdown) {
            throw new IllegalStateException("Exception occurred shutting down group");
        }
    }

    /**
     * Shutdown cluster member groups and the cache factory.
     *
     * @param memberGroups Member groups.
     */
    public static void shutdownClusterMemberGroupsThenCacheFactory(ClusterMemberGroup... memberGroups) {
        try {
            shutdownClusterMemberGroups(memberGroups);
        } finally {
            CacheFactory.shutdown();
        }
    }

    /**
     * Shutdown cache factory and then cluster member groups.
     *
     * @param memberGroups Member groups.
     */
    public static void shutdownCacheFactoryThenClusterMemberGroups(ClusterMemberGroup... memberGroups) {
        try {
            CacheFactory.shutdown();
        } finally {
            shutdownClusterMemberGroups(memberGroups);
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
    public static Properties createExtendProxyServerPropertiesWithDefaults() {
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
        final String port = System.getProperty(CoherenceSystemPropertyConst.WKA_PORT_KEY, Integer.toString(CoherenceSystemPropertyConst.DEFAULT_WKA_PORT));
        final String log = System.getProperty(CoherenceSystemPropertyConst.LOG_KEY, CoherenceSystemPropertyConst.DEFAULT_LOG);
        final String logLevel = System.getProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, Integer.toString(CoherenceSystemPropertyConst.DEFAULT_LOG_LEVEL));

        PropertyContainer container = new PropertyContainer();
        container.addProperty(CoherenceSystemPropertyConst.TCMP_ENABLED_KEY, Boolean.TRUE.toString());
        container.addProperty(CoherenceSystemPropertyConst.CLUSTER_KEY, "TestLocalProcessCluster");
        container.addProperty(CoherenceSystemPropertyConst.LOG_LEVEL_KEY, logLevel);
        container.addProperty(CoherenceSystemPropertyConst.LOG_KEY, log);
        container.addProperty(CoherenceSystemPropertyConst.WKA_KEY, host);
        container.addProperty(CoherenceSystemPropertyConst.WKA_PORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.LOCALHOST_KEY, host);
        container.addProperty(CoherenceSystemPropertyConst.LOCALPORT_KEY, port);
        container.addProperty(CoherenceSystemPropertyConst.TTL_KEY, "0");
//        container.addProperty(MANAGEMENT_KEY, "all");
//        container.addProperty(MANAGEMENT_REMOTE_KEY, Boolean.TRUE.toString());
//        container.addProperty(JMXREMOTE_KEY, "");

        return container;
    }
}
