package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import org.testsupport.coherence.impl.DefaultClusterMemberGroupBuilder;
import org.testsupport.common.lang.PropertyContainer;
import org.testsupport.common.lang.SystemUtils;

import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * Cluster member group factory.
 */
public final class ClusterMemberGroupUtils {
    private static final Logger LOGGER = Logger.getLogger(ClusterMemberGroupUtils.class.getName());

    private static final float COHERENCE_VERSION_3_5 = 3.5f;
    private static final float COHERENCE_VERSION_3_6 = 3.6f;
    private static final float COHERENCE_VERSION_3_7 = 3.7f;
    private static final String COHERENCE_VERSION_3_7_0 = "3.7.0";

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_PRE_3_5 = 60;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_5 = 45;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_6 = 3;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_0 = 3;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_1_OR_LATER = 3;

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_PRE_3_5 = 1;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_5 = 1;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_6 = 1;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_7_0 = 0;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_7_1_OR_LATER = 0;

    /**
     * Private constructor to prevent creation.
     */
    private ClusterMemberGroupUtils() {
    }

    /**
     * Creates a new builder to construct a cluster member group.
     *
     * @return builder.
     */
    public static ClusterMemberGroup.Builder newClusterMemberGroupBuilder() {
        return new DefaultClusterMemberGroupBuilder();
    }

    /**
     * Returns the sleep time based upon Coherence version in which to sleep after a member shutdown.
     *
     * @return sleep time.
     */
    public static int getSecondsToSleepAfterPerformingMemberShutdown() {
        final float majorMinorVersion = getMajorMinorVersion();

        if (majorMinorVersion < COHERENCE_VERSION_3_5) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_PRE_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_3_6) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_3_7) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_6;

        } else {
            if (CacheFactory.VERSION.startsWith(COHERENCE_VERSION_3_7_0)) {
                return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_7_0;
            }
        }

        return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN_FOR_VERSION_3_7_1_OR_LATER;
    }

    /**
     * Returns the sleep time based upon Coherence version in which to sleep after a member has been stopped.
     *
     * @return sleep time.
     */
    public static int getSecondsToSleepAfterPerformingMemberStop() {
        final float majorMinorVersion = getMajorMinorVersion();

        if (majorMinorVersion < COHERENCE_VERSION_3_5) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_PRE_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_3_6) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_3_7) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_6;

        } else {
            if (CacheFactory.VERSION.startsWith(COHERENCE_VERSION_3_7_0)) {
                return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_0;
            }
        }

        return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_1_OR_LATER;
    }

    private static float getMajorMinorVersion() {
        final String majorMinorVersionString = CacheFactory.VERSION.substring(0, 3);

        return Float.parseFloat(majorMinorVersionString);
    }

    /**
     * Sleeps for a period of time (dependent upon Coherence version) after a member has been shutdown.
     */
    public static void sleepAfterPerformingMemberShutdown() {
        sleepForSeconds(getSecondsToSleepAfterPerformingMemberShutdown());
    }

    /**
     * Sleeps for a period of time (dependent upon Coherence version) after a member has been stopped.
     */
    public static void sleepAfterPerformingMemberStop() {
        sleepForSeconds(getSecondsToSleepAfterPerformingMemberStop());
    }

    private static void sleepForSeconds(final int seconds) {
        LOGGER.info(format("Coherence '%s' - will now sleep for '%s' seconds to allow member left to be acknowledged",
                CacheFactory.VERSION, seconds));

        try {
            SECONDS.sleep(seconds);
        } catch (InterruptedException e) {
            throw new IllegalStateException(e);
        }
    }

    //TODO: Think about JMX
//            properties.addSystemProperty(MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMXREMOTE_KEY, "");

    /**
     * Shutdown cluster member groups.
     *
     * @param clusterMemberGroups Member groups.
     */
    public static void shutdownClusterMemberGroups(final ClusterMemberGroup... clusterMemberGroups) {
        boolean exceptionOccurredDuringShutdown = false;

        for (ClusterMemberGroup clusterMemberGroup : clusterMemberGroups) {
            try {
                if (clusterMemberGroup != null) {
                    clusterMemberGroup.shutdownAll();
                }
            } catch (Exception e) {
                exceptionOccurredDuringShutdown = true;

                LOGGER.severe("Could not shutdown cluster member group: " + e);
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
    public static void shutdownClusterMemberGroupsThenCacheFactory(final ClusterMemberGroup... memberGroups) {
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
    public static void shutdownCacheFactoryThenClusterMemberGroups(final ClusterMemberGroup... memberGroups) {
        try {
            CacheFactory.shutdown();
        } finally {
            shutdownClusterMemberGroups(memberGroups);
        }
    }

    /**
     * Sets the system properties for a storage-disabled client.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties() {
        setStorageDisabledClientSystemProperties(null, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties(String cacheConfiguration) {
        setStorageDisabledClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties(String cacheConfiguration,
                                                                Properties properties) {

        PropertyContainer containerToUse = internalCreateStorageDisabledClientPropertyContainerWithDefaults();
        containerToUse.addProperties(properties);

        internalSetGenericClientSystemProperties(cacheConfiguration, containerToUse);
    }

    /**
     * Sets the system properties for an Extend client.
     *
     * @param cacheConfiguration Cache configuration.
     */
    @Deprecated
    public static void setExtendClientSystemProperties(String cacheConfiguration) {
        setExtendClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for an Extend client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
    @Deprecated
    public static void setExtendClientSystemProperties(String cacheConfiguration,
                                                       Properties properties) {

        PropertyContainer containerToUse = internalCreateExtendClientPropertyContainerWithDefaults();
        containerToUse.addProperties(properties);

        internalSetGenericClientSystemProperties(cacheConfiguration, containerToUse);
    }

    @Deprecated
    private static void internalSetGenericClientSystemProperties(String cacheConfiguration,
                                                                 PropertyContainer propertyContainer) {

        PropertyContainer containerToUse = new PropertyContainer(propertyContainer);
        containerToUse.addProperty(CoherenceSystemPropertyConst.CACHECONFIG_KEY, cacheConfiguration);

        outputAndSetClientSystemProperties(containerToUse);
    }


    @Deprecated
    private static void outputAndSetClientSystemProperties(PropertyContainer propertyContainer) {
        LOGGER.fine(format("Client current Coherence properties: %s ",
                SystemUtils.getSystemPropertiesWithPrefix(CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT)));

        LOGGER.fine(format("Client system properties to set: %s", propertyContainer));

        SystemUtils.setReplaceClearSystemProperties(propertyContainer);
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createStorageDisabledClientPropertiesWithDefaults() {
        return internalCreateStorageDisabledClientPropertyContainerWithDefaults().getProperties();

    }

    @Deprecated
    private static PropertyContainer internalCreateStorageDisabledClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(DefaultClusterMemberGroupBuilder.createCacheServerPropertiesWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.ROLE_KEY, "StorageDisabledClient");
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());

        return container;
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createExtendClientPropertiesWithDefaults() {
        return internalCreateExtendClientPropertyContainerWithDefaults().getProperties();
    }

    @Deprecated
    private static PropertyContainer internalCreateExtendClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(DefaultClusterMemberGroupBuilder.createGenericClusterMemberPropertiesWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());
        container.addProperty(CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        container.addProperty(CoherenceSystemPropertyConst.TCMP_ENABLED_KEY, Boolean.FALSE.toString());

        return container;
    }
}
