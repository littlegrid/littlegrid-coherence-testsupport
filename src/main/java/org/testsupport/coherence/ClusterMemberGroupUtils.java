package org.testsupport.coherence;

import com.tangosol.net.CacheFactory;
import org.testsupport.coherence.impl.DefaultClusterMemberGroupBuilder;
import org.testsupport.common.LoggerWrapper;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * Cluster member group factory.
 */
public final class ClusterMemberGroupUtils {
    private static final LoggerWrapper LOGGER = new LoggerWrapper(ClusterMemberGroupUtils.class.getName());

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
}