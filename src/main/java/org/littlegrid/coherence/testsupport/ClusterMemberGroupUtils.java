package org.littlegrid.coherence.testsupport;

import com.tangosol.net.CacheFactory;
import org.littlegrid.coherence.testsupport.impl.DefaultClusterMemberGroupBuilder;
import org.littlegrid.common.LoggerPlaceHolder;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * Cluster member group factory.
 */
public final class ClusterMemberGroupUtils {
    private static final String COHERENCE_SLEEP_PROPERTIES_FILENAME = "coherence/littlegrid-coherence-sleep.properties";
    private static final LoggerPlaceHolder LOGGER = new LoggerPlaceHolder(ClusterMemberGroupUtils.class.getName());
    private static final float COHERENCE_VERSION_NUMBER_3_5 = 3.5f;
    private static final float COHERENCE_VERSION_NUMBER_3_6 = 3.6f;
    private static final float COHERENCE_VERSION_NUMBER_3_7 = 3.7f;
    private static final String COHERENCE_VERSION_3_7_0 = "3.7.0";
    private static final Properties SLEEP_PROPERTIES = new Properties();

    static {
        InputStream stream = ClusterMemberGroupUtils.class.getClassLoader()
                .getResourceAsStream(COHERENCE_SLEEP_PROPERTIES_FILENAME);

        if (stream == null) {
            String message = format("Unable to load '%s'", COHERENCE_SLEEP_PROPERTIES_FILENAME);

            LOGGER.error(message);
            throw new IllegalStateException(message);
        }

        try {
            SLEEP_PROPERTIES.load(stream);
        } catch (IOException e) {
            String message = format("Problem with loading Coherence sleep properties");

            LOGGER.error(message);
            throw new IllegalStateException(message);
        }
    }

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_PRE_3_5 =
            Integer.parseInt(SLEEP_PROPERTIES.getProperty("sleep-after-stop-pre-3.5", "60"));

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_5 =
            Integer.parseInt(SLEEP_PROPERTIES.getProperty("sleep-after-stop-3.5", "45"));

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_6 =
            Integer.parseInt(SLEEP_PROPERTIES.getProperty("sleep-after-stop-3.6", "3"));

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_0 =
            Integer.parseInt(SLEEP_PROPERTIES.getProperty("sleep-after-stop-3.7.0", "3"));

    private static final int SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_7_1_OR_LATER =
            Integer.parseInt(SLEEP_PROPERTIES.getProperty("sleep-after-stop-3.7.1-or-later", "3"));


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
     * Returns the sleep time based upon Coherence version in which to sleep after a member has been stopped.
     *
     * @return sleep time.
     */
    public static int getSecondsToSleepAfterPerformingMemberStop() {
        final float majorMinorVersion = getMajorMinorVersion();

        if (majorMinorVersion < COHERENCE_VERSION_NUMBER_3_5) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_PRE_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_NUMBER_3_6) {
            return SECONDS_TO_SLEEP_AFTER_PERFORMING_STOP_FOR_VERSION_3_5;

        } else if (majorMinorVersion < COHERENCE_VERSION_NUMBER_3_7) {
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
     * Sleeps for a period of time (dependent upon Coherence version) after a member has been stopped.
     */
    public static void sleepAfterPerformingMemberStop() {
        sleepForSeconds(getSecondsToSleepAfterPerformingMemberStop());
    }

    private static void sleepForSeconds(final int seconds) {
        LOGGER.info(format(
                "Coherence '%s' - so will now sleep for '%s' seconds to allow the member left to be acknowledged",
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
