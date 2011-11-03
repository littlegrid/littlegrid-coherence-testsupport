package org.testsupport.coherence;

import com.tangosol.net.Member;

/**
 * Cluster member interface - implementations of this class need to provide basic functionality,
 * so they may be controlled by the {@link org.testsupport.coherence.ClusterMemberGroup}
 * implementations - typically the default implementation of this class should suffice for most
 * uses.
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
