package com.practicalblend.coherence.testsupport.server;

/**
 * Cluster member group, a collection of cluster members enabling them to be controlled as a group or
 * individually for some operations.
 */
public interface ClusterMemberGroup {
    /**
     * Starts all the cluster members in the group.
     */
    void startAll();

    /**
     * Shuts down a specific cluster members in the group.
     *
     * @param memberId Id of cluster member to shutdown.
     * @return boolean indicating if member was shutdown.
     */
    boolean shutdown(int memberId);

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
    boolean stop(int memberId);

    /**
     * Stops a specific cluster member.
     *
     * @param memberId Ids of cluster members to stop.
     * @return Array of boolean results indicating for each Id if it was stopped.
     */
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
    ClusterMemberGroupStatus getStatus();
}
