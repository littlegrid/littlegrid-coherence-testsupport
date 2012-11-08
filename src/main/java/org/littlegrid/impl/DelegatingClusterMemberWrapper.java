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
 * Neither the name of the littlegrid nor the names of its contributors may
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

package org.littlegrid.impl;

import com.tangosol.util.ClassHelper;
import com.tangosol.util.WrapperException;
import org.littlegrid.IdentifiableException;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.support.ChildFirstUrlClassLoader;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.IdentifiableException.ReasonEnum.CHECK_CACHE_CONFIGURATION_FILE_BEING_USED;
import static org.littlegrid.IdentifiableException.ReasonEnum.SUSPECTED_AUTOSTART_EXCEPTION;

/**
 * Delegating cluster member wrapper, loads a class that implements {@link
 * org.littlegrid.ClusterMemberGroup.ClusterMember}
 * into a separate class loader and then delegates requests (start, stop, shutdown etc.) to
 * the instance of the wrapped class.
 */
class DelegatingClusterMemberWrapper implements ClusterMemberGroup.ClusterMember {
    private static final Logger LOGGER = Logger.getLogger(DelegatingClusterMemberWrapper.class.getName());

    private final Object clusterMemberInstance;
    private boolean running = false;


    /**
     * Constructor.
     *
     * @param clusterMemberInstanceClassName Name of class to instantiate and delegate calls to.
     * @param childFirstUrlClassLoader       Instance of child first class loader.
     */
    public DelegatingClusterMemberWrapper(final String clusterMemberInstanceClassName,
                                          final ChildFirstUrlClassLoader childFirstUrlClassLoader) {
        try {
            LOGGER.fine(format("Cluster member class to be instantiated: '%s'", clusterMemberInstanceClassName));

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
        LOGGER.fine("About to start this cluster member");

        invokeMethod(clusterMemberInstance, "start");

        running = true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        running = false;

        LOGGER.fine("Shutting down this cluster member");

        invokeMethod(clusterMemberInstance, "shutdown");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        running = false;

        LOGGER.fine("Stopping this cluster member");

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
            Throwable originalCause = e;

            while (originalCause.getCause() != null) {
                originalCause = originalCause.getCause();
            }

            if (originalCause.getMessage().contains("Error instantiating Filter with name: gzip")) {
                throw new IdentifiableException(
                        "Please check that at least one of your caches is marked with <autostart>true</autostart> "
                                + "in the cache configuration file - this is a current littlegrid limitation "
                                + "",
                        e, SUSPECTED_AUTOSTART_EXCEPTION);
            }

            throw new IllegalStateException(e);
        }
    }

    /**
     * Determines if the member is 'running', it is  running if it has been successfully
     * started and stop or shutdown have not been invoked against it.
     *
     * @return true if running.
     */

    boolean isRunning() {
        return running;
    }
}
