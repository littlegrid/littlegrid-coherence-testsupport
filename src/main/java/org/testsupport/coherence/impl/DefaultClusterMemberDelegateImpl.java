package org.testsupport.coherence.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.DefaultCacheServer;
import com.tangosol.net.Member;

/**
 * Default cluster member delegate implementation that performs the necessary cluster member actions - this
 * implementation simply delegates to a Default cache server where possible.
 */
public class DefaultClusterMemberDelegateImpl implements ClusterMember {
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
        CacheFactory.log("*************** Shutdown");
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
    public Member getLocalMember() {
        throw new UnsupportedOperationException();
    }
}
