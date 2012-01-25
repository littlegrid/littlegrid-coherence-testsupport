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

import java.net.URL;
import java.util.concurrent.Callable;

/**
 * Local process cluster member callable, a task that starts a cluster member wrapper.
 */
class ClusterMemberCallable implements Callable<DelegatingClusterMemberWrapper> {
    private URL[] classPathUrls;
    private String clusterMemberInstanceClassName;

    /**
     * Constructor.
     *
     * @param clusterMemberInstanceClassName Cluster member class name.
     * @param classPathUrls                  Class path.
     */
    public ClusterMemberCallable(final String clusterMemberInstanceClassName,
                                 final URL[] classPathUrls) {

        if (clusterMemberInstanceClassName == null) {
            throw new IllegalStateException("Cluster member class name cannot be null");
        }

        if (classPathUrls == null) {
            throw new IllegalStateException("Class path URLs cannot be null");
        }

        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;
        this.classPathUrls = classPathUrls;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DelegatingClusterMemberWrapper call()
            throws Exception {

        final ClassLoader originalClassLoader = Thread.currentThread().getContextClassLoader();

        try {
            final ChildFirstUrlClassLoader childFirstUrlClassLoader = new ChildFirstUrlClassLoader(classPathUrls);
            Thread.currentThread().setContextClassLoader(childFirstUrlClassLoader);

            final DelegatingClusterMemberWrapper memberWrapper =
                    new DelegatingClusterMemberWrapper(clusterMemberInstanceClassName, childFirstUrlClassLoader);

            memberWrapper.start();

            return memberWrapper;
        } catch (Throwable throwable) {
            throw new Exception(throwable);
        } finally {
            Thread.currentThread().setContextClassLoader(originalClassLoader);
        }
    }
}
