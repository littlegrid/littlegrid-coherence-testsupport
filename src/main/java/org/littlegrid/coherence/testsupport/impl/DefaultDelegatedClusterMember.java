/*
 * Copyright (c) 2011, Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the LittleGrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid.coherence.testsupport.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.DefaultCacheServer;
import org.littlegrid.coherence.testsupport.ClusterMember;

/**
 * Default delegated cluster member (which may be extended if specialised behaviour is required,
 * such as before start-up etc.), it performs the necessary cluster member actions - this
 * implementation simply delegates to a Default cache server where possible.
 */
public class DefaultDelegatedClusterMember implements ClusterMember {
    /**
     * Performs any necessary setup before cluster member is started.
     */
    public void doBeforeStart() {
    }

    /**
     * Start the cluster member, the start method is purposely not on cluster member interface to
     * prevent normal framework users from calling it from their test code - thus generally
     * avoiding things like multiple invocations of start.
     */
    public void start() {
        doBeforeStart();
        DefaultCacheServer.start();
        doAfterStart();
    }

    /**
     * Performs any necessary actions after the cluster member has been started.
     */
    public void doAfterStart() {
    }

    /**
     * Performs any necessary actions before the cluster member is shutdown.
     */
    public void doBeforeShutdown() {
    }

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
    public void doAfterShutdown() {
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
