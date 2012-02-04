/*
 * Copyright (c) 2010-2012 Jonathan Hall.
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

import com.tangosol.util.ClassHelper;
import org.littlegrid.coherence.testsupport.ClusterMember;
import org.littlegrid.utils.ChildFirstUrlClassLoader;
import org.littlegrid.utils.LoggerPlaceHolder;

import java.lang.reflect.Constructor;

import static java.lang.String.format;

/**
 * Delegating cluster member wrapper, loads a class that implements {@link ClusterMember}
 * into a separate class loader and then delegates requests (start, stop, shutdown etc.) to
 * the instance of the wrapped class.
 */
class DelegatingClusterMemberWrapper implements ClusterMember {
    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(DelegatingClusterMemberWrapper.class.getName());

    private final Object clusterMemberInstance;


    /**
     * Constructor.
     *
     * @param clusterMemberInstanceClassName Name of class to instantiate and delegate calls to.
     * @param childFirstUrlClassLoader       Instance of child first class loader.
     */
    public DelegatingClusterMemberWrapper(final String clusterMemberInstanceClassName,
                                          final ChildFirstUrlClassLoader childFirstUrlClassLoader) {
        try {
            LOGGER.debug(format("Cluster member class to be instantiated: '%s'", clusterMemberInstanceClassName));

            final Class clusterMemberClass = childFirstUrlClassLoader.loadClass(clusterMemberInstanceClassName);
            final Constructor constructor = clusterMemberClass.getConstructor();

            clusterMemberInstance = constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Start the cluster member - this has reduced scope to prevent normal framework users from calling it.
     */
    void start() {
        LOGGER.debug("About to start this cluster member");

        invokeMethod(clusterMemberInstance, "start");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        LOGGER.debug("Shutting down this cluster member");

        invokeMethod(clusterMemberInstance, "shutdown");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        LOGGER.debug("Stopping this cluster member");

        invokeMethod(clusterMemberInstance, "stop");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLocalMemberId() {
        return (Integer) invokeMethod(clusterMemberInstance, "getLocalMemberId");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClassLoader getActualContainingClassLoader() {
        return (ClassLoader) invokeMethod(clusterMemberInstance, "getActualContainingClassLoader");
    }

    private Object invokeMethod(final Object objectToInvokeMethodOn,
                                final String methodName) {

        try {
            return ClassHelper.invoke(objectToInvokeMethodOn, methodName, new Object[]{});
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }
}
