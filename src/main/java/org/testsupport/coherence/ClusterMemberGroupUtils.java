package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import org.testsupport.coherence.impl.DefaultClusterMemberGroupBuilder;

import java.util.logging.Logger;

import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * Cluster member group factory.
 */
public class ClusterMemberGroupUtils {
    private static final Logger LOGGER = Logger.getLogger(ClusterMemberGroupUtils.class.getName());

    //TODO: Double check if logging is required
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP = 3;
    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN = 1;

    /**
     * Private constructor to prevent creation.
     */
    private ClusterMemberGroupUtils() {
    }

    public static ClusterMemberGroup.Builder newBuilder() {
        return new DefaultClusterMemberGroupBuilder();
    }

    public static int getSecondsToSleepAfterPerformingMemberShutdown() {
        return SECONDS_TO_SLEEP_AFTER_PERFORMING_SHUTDOWN;
    }

    public static int getSecondsToSleepAfterPerformingMemberStop() {
        return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP;
    }

    public static void sleepAfterPerformingMemberShutdown() {
        sleepForSeconds(getSecondsToSleepAfterPerformingMemberShutdown());
    }

    public static void sleepAfterPerformingMemberStop() {
        sleepForSeconds(getSecondsToSleepAfterPerformingMemberStop());
    }

    private static void sleepForSeconds(int seconds) {
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
    public static void shutdownClusterMemberGroups(ClusterMemberGroup... clusterMemberGroups) {
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
}
