package org.littlegrid.coherence.testsupport.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.DefaultCacheServer;
import org.littlegrid.coherence.testsupport.ClusterMember;

/**
 * Abstract class for delegated cluster member to extend, it performs the necessary
 * cluster member actions - this  implementation simply delegates to a Default cache
 * server where possible.
 */
public abstract class AbstractDelegatedClusterMember implements ClusterMember {
    /**
     * Performs any necessary setup before cluster member is started.
     */
    public abstract void doBeforeStart();

    /**
     * Start the cluster member - this has reduced scope to prevent normal framework
     * users from calling it.
     */
    public void start() {
        doBeforeStart();
        DefaultCacheServer.start();
        doAfterStart();
    }

    /**
     * Performs any necessary actions after the cluster member has been started.
     */
    public abstract void doAfterStart();

    /**
     * Performs any necessary actions before the cluster member is shutdown.
     */
    public abstract void doBeforeShutdown();

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        doBeforeShutdown();
        DefaultCacheServer.shutdown();
        doAfterShutdown();
    }

    /**
     * Performs any necessary actions after the cluster member has been shutdown.
     */
    public abstract void doAfterShutdown();

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
