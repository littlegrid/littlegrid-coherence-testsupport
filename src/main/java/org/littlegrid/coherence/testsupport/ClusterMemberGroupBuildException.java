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

package org.littlegrid.coherence.testsupport;

import java.net.URL;
import java.util.Properties;

/**
 * Cluster member group exception.
 */
public class ClusterMemberGroupBuildException extends RuntimeException {
    private Properties systemPropertiesBeforeStartInvoked;
    private Properties systemPropertiesToBeApplied;
    private int numberOfMembers;
    private URL[] classPathUrls;
    private String clusterMemberInstanceClassName;
    private int numberOfThreadsInStartUpPool;

    /**
     * Constructor.
     *
     * @param cause                          Original exception.
     * @param systemPropertiesBeforeStart    System properties before being changed.
     * @param systemPropertiesToBeApplied    System properties to be applied.
     * @param numberOfMembers                Number of members to start.
     * @param classPathUrls                  Class paths defined for use.
     * @param clusterMemberInstanceClassName Cluster member instance class name.
     * @param numberOfThreadsInStartUpPool   Number of threads in start-up pool.
     */
    public ClusterMemberGroupBuildException(final Throwable cause,
                                            final Properties systemPropertiesBeforeStart,
                                            final Properties systemPropertiesToBeApplied,
                                            final int numberOfMembers,
                                            final URL[] classPathUrls,
                                            final String clusterMemberInstanceClassName,
                                            final int numberOfThreadsInStartUpPool) {

        super(cause);

        this.systemPropertiesBeforeStartInvoked = systemPropertiesBeforeStart;
        this.systemPropertiesToBeApplied = systemPropertiesToBeApplied;
        this.numberOfMembers = numberOfMembers;
        this.classPathUrls = classPathUrls;
        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;
    }

    public Properties getSystemPropertiesBeforeStartInvoked() {
        return systemPropertiesBeforeStartInvoked;
    }

    public Properties getSystemPropertiesToBeApplied() {
        return systemPropertiesToBeApplied;
    }

    public int getNumberOfMembers() {
        return numberOfMembers;
    }

    public URL[] getClassPathUrls() {
        return classPathUrls;
    }

    public String getClusterMemberInstanceClassName() {
        return clusterMemberInstanceClassName;
    }

    public int getNumberOfThreadsInStartUpPool() {
        return numberOfThreadsInStartUpPool;
    }
}
