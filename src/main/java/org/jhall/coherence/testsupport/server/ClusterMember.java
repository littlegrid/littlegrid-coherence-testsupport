package org.jhall.coherence.testsupport.server;

/**
 * Cluster member interface - implementations of this class need to provide basic functionality, so they
 * may be controlled by the {@link ClusterMemberGroup} implementations - typically the default implementation
 * of this class should suffice for most uses.
 */
public interface ClusterMember {
    /**
     * Starts the member up.
     */
    void start();

    /**
     * Shutdown the member.
     */
    void shutdown();

    /**
     * Stops the member.
     */
    void stop();

    /**
     * Gets the local member id.
     *
     * @return this member id.
     */
    int getLocalMemberId();
}
