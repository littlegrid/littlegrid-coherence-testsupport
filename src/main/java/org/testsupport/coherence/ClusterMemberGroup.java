package org.testsupport.coherence;

import java.util.Properties;

/**
 * Cluster member group, a collection of cluster members enabling them to be controlled as a group or
 * individually for some operations.
 */
public interface ClusterMemberGroup {
    /**
     * Starts all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup startAll();

    /**
     * Shuts down a specific cluster members in the group.
     *
     * @param memberId Id of cluster member to shutdown.
     * @return member group.
     */
    @Deprecated
    ClusterMemberGroup shutdownMember(int memberId);

    /**
     * Shuts down all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup shutdownAll();

    /**
     * Stops a specific cluster member.
     *
     * @param memberId Id of cluster member to stop.
     * @return member group.
     */
    @Deprecated
    ClusterMemberGroup stopMember(int memberId);

    /**
     * Stops all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup stopAll();

    static interface Builder {
        enum Topology {
            STORAGE_ENABLED_ONLY,
            COMPOSITE_STORAGE_ENABLED_PROXY,
            PROXY_AND_SEPARATE_STORAGE_ENABLED,
            EXTEND_PROXY_ONLY
        }

        ClusterMemberGroup build();

        Builder setCacheConfiguration(String cacheConfiguration);

        Builder setSystemProperties(Properties properties);

        Builder setTopology(Topology topology);

        Builder setMemberCount(int numberOfMembers);
    }
}
