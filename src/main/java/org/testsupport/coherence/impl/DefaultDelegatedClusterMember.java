package org.testsupport.coherence.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.DefaultCacheServer;
import org.testsupport.coherence.ClusterMember;

/**
 * Default cluster member delegate implementation that performs the necessary cluster member actions - this
 * implementation simply delegates to a Default cache server where possible.
 */
public final class DefaultDelegatedClusterMember implements ClusterMember {
    /**
     * {@inheritDoc}
     */
    @Override
    public void start() {
        DefaultCacheServer.start();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        DefaultCacheServer.shutdown();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        CacheFactory.getCluster().stop();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLocalMemberId() {
        return CacheFactory.getCluster().getLocalMember().getId();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClassLoader getActualContainingClassLoader() {
        return this.getClass().getClassLoader();
    }
}
