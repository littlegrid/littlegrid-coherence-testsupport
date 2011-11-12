package org.littlegrid.coherence;

/**
 * Cluster member interface - implementations of this class need to provide basic functionality,
 * so they may be controlled by the {@link ClusterMemberGroup}
 * implementations - typically the default implementation of this class should suffice for most
 * uses.
 */
public interface ClusterMember {
    /**
     * Shutdown the member.
     */
    void shutdown();

    /**
     * Stops the member.
     */
    void stop();

    /**
     * Gets this local member Id.
     *
     * @return member id.
     */
    int getLocalMemberId();

    /**
     * Returns the class loader that the cluster member has been loaded into.
     *
     * @return class loader.
     */
    ClassLoader getActualContainingClassLoader();
}
