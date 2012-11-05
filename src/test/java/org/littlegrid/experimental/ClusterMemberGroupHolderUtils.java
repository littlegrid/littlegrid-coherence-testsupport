package org.littlegrid.experimental;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * This is an experimental feature and is likely to change in the future!
 *
 * @since 2.11
 */
final class ClusterMemberGroupHolderUtils {
    private ClusterMemberGroupHolderUtils() {
    }

    private static final Logger LOGGER = Logger.getLogger(ClusterMemberGroupHolderUtils.class.getName());

    private static Map<Class, HolderBuilderCountingContainer> builderContainers =
            new HashMap<Class, HolderBuilderCountingContainer>();

    public static synchronized ClusterMemberGroupHolder.Builder newBuilder(final Class clusterMemberGroupHolderClass) {
        HolderBuilderCountingContainer builderContainer = builderContainers.get(clusterMemberGroupHolderClass);

        if (builderContainer == null) {
            LOGGER.info(format("Creating builder container for '%s'", clusterMemberGroupHolderClass));

            builderContainer = new HolderBuilderCountingContainer(
                    new ClusterMemberGroupHolder.DefaultClusterMemberGroupHolderBuilder(clusterMemberGroupHolderClass));

            builderContainers.put(clusterMemberGroupHolderClass, builderContainer);
        } else {
            LOGGER.fine(format("Builder container for '%s', already exists and doesn't need to be created",
                    clusterMemberGroupHolderClass));
        }

        final int currentCount = builderContainer.incrementCounterAndGet();
        LOGGER.fine(format("Builder container count for '%s' is now %s",
                clusterMemberGroupHolderClass, currentCount));

        return builderContainer.getBuilderHolder();
    }

    public static synchronized void shutdownClusterMemberGroupHolders(
            final ClusterMemberGroupHolder... clusterMemberGroupHolders) {

        for (final ClusterMemberGroupHolder groupHolder : clusterMemberGroupHolders) {
            final Class clusterMemberGroupHolderClass = groupHolder.getClass();
            final HolderBuilderCountingContainer builderContainer =
                    builderContainers.get(clusterMemberGroupHolderClass);

            final int currentCount = builderContainer.decrementCounter();

            if (currentCount == 0) {
                LOGGER.info(format("Builder container count for '%s' is now zero and will be shutdown",
                        clusterMemberGroupHolderClass));

                groupHolder.shutdown();

                builderContainers.remove(clusterMemberGroupHolderClass);
            } else if (currentCount > 0) {
                LOGGER.fine(format("Builder container count for '%s' is now %s - deferring shutdown",
                        clusterMemberGroupHolderClass, currentCount));
            } else {
                throw new IllegalStateException(format(
                        "Builder container '%s' counter has already been decremented to zero and been shutdown",
                        clusterMemberGroupHolderClass));
            }
        }
    }

    private static class HolderBuilderCountingContainer {
        private ClusterMemberGroupHolder.Builder builderHolder;
        private AtomicInteger counter = new AtomicInteger();

        private HolderBuilderCountingContainer(final ClusterMemberGroupHolder.Builder builderHolder) {
            this.builderHolder = builderHolder;
        }

        public int incrementCounterAndGet() {
            return counter.incrementAndGet();
        }

        public int decrementCounter() {
            return counter.decrementAndGet();
        }

        public ClusterMemberGroupHolder.Builder getBuilderHolder() {
            return builderHolder;
        }
    }
}
