package org.sprinkletest.coherence.support;

import org.sprinkletest.coherence.support.server.ClusterMemberGroupStatus;

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
     * @return boolean indicating if member was shutdown.
     */
    @Deprecated
    boolean shutdownMember(int memberId);

    /**
     * Shuts down all the cluster members in the group.
     */
    void shutdownAll();

    /**
     * Stops a specific cluster member.
     *
     * @param memberId Id of cluster member to stop.
     * @return boolean indicating if member was stopped.
     */
    @Deprecated
    boolean stopMember(int memberId);

    /**
     * Stops a specific cluster member.
     *
     * @param memberId Ids of cluster members to stop.
     * @return Array of boolean results indicating for each Id if it was stopped.
     */
    @Deprecated
    boolean[] stop(Integer[] memberId);

    /**
     * Stops all the cluster members in the group.
     */
    void stopAll();

    /**
     * Returns current life-cycle status of member group.
     *
     * @return status.
     */
    @Deprecated
    ClusterMemberGroupStatus getStatus();

    static interface Builder {
        ClusterMemberGroup build();

        ClusterMemberGroup setStorageEnabledCount(int numberOfMembers);

        ClusterMemberGroup setExtendProxyCount(int numberOfMembers);

        ClusterMemberGroup setCompositeStorageEnabledProxyCount(int numberOfMembers);

        ClusterMemberGroup setCacheConfiguration(String cacheConfiguration);

        ClusterMemberGroup setSystemProperties(Properties properties);
    }
}
