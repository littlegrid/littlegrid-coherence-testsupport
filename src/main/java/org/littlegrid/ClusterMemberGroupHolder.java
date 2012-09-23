package org.littlegrid;

import java.lang.reflect.Constructor;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * This is an experimental feature and is likely to change in the future!
 *
 * @since 2.11
 */
interface ClusterMemberGroupHolder {
    void start();

    void shutdown();

    ClusterMemberGroup getClusterMemberGroup();


    /**
     * @since 2.11
     */
    interface Builder {
        ClusterMemberGroupHolder build();
    }

    public class DefaultClusterMemberGroupHolderBuilder implements Builder {
        private static final Logger LOGGER = Logger.getLogger(DefaultClusterMemberGroupHolderBuilder.class.getName());

        private Class clusterMemberGroupHolderClass;
        private ClusterMemberGroupHolder groupHolder;

        public DefaultClusterMemberGroupHolderBuilder(final Class clusterMemberGroupHolderClass) {
            this.clusterMemberGroupHolderClass = clusterMemberGroupHolderClass;
        }

        @Override
        public ClusterMemberGroupHolder build() {
            if (clusterMemberGroupHolderClass == null) {
                throw new IllegalStateException("Cluster member group holder class has not been set");
            }

            if (groupHolder != null) {
                LOGGER.fine(format("This holder for '%s' has already been built so existing instance will be used",
                        clusterMemberGroupHolderClass));

                return groupHolder;
            }

            final String className = clusterMemberGroupHolderClass.getName();

            try {
                LOGGER.info(format("About to create and start a holder for '%s'", clusterMemberGroupHolderClass));

                final Class clazz = this.getClass().getClassLoader().loadClass(className);
                final Constructor constructor = clazz.getConstructor();

                groupHolder = (ClusterMemberGroupHolder) constructor.newInstance();
                groupHolder.start();

                return groupHolder;
            } catch (Exception e) {
                throw new IllegalStateException(format("Cannot create instance of '%s", className));
            }
        }
    }

    public class DefaultClusterMemberGroupHolder implements ClusterMemberGroupHolder {
        private ClusterMemberGroup memberGroup;

        @Override
        public void start() {
            memberGroup = ClusterMemberGroupUtils.newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();
        }

        @Override
        public void shutdown() {
            ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }

        @Override
        public ClusterMemberGroup getClusterMemberGroup() {
            return memberGroup;
        }
    }
}
