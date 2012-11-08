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

package org.littlegrid;

import com.tangosol.net.CacheFactory;
import org.littlegrid.impl.DefaultClusterMemberGroupBuilder;

import java.io.IOException;

import static java.lang.String.format;

/**
 * Cluster member group utilities.
 */
public final class ClusterMemberGroupUtils {
    /**
     * Private constructor to prevent creation.
     */
    private ClusterMemberGroupUtils() {
    }

    /**
     * Creates a new builder to construct a cluster member group.
     *
     * @return builder.
     */
    public static ClusterMemberGroup.Builder newBuilder() {
        return new DefaultClusterMemberGroupBuilder();
    }

    /**
     * Shutdown cluster member groups.
     *
     * @param clusterMemberGroups Member groups.
     */
    public static void shutdownClusterMemberGroups(final ClusterMemberGroup... clusterMemberGroups) {
        if (clusterMemberGroups == null) {
            return;
        }

        int exceptionDuringShutdownCounter = 0;

        for (final ClusterMemberGroup clusterMemberGroup : clusterMemberGroups) {
            try {
                if (clusterMemberGroup != null) {
                    clusterMemberGroup.shutdownAll();
                }
            } catch (Exception e) {
                exceptionDuringShutdownCounter++;

                // Ignore for now and carry on looping to try and shutdown any other
                // cluster member groups that may be running
            }
        }

        if (exceptionDuringShutdownCounter > 0) {
            throw new IllegalStateException(
                    format("Whilst attempting to shutdown the total of %s member group(s), "
                            + "%s of them threw exceptions during their shutdown",
                            clusterMemberGroups.length, exceptionDuringShutdownCounter));
        }
    }

    /**
     * Shutdown the cache factory and then the cluster member groups.
     *
     * @param memberGroups Member groups.
     */
    public static void shutdownCacheFactoryThenClusterMemberGroups(final ClusterMemberGroup... memberGroups) {
        try {
            CacheFactory.shutdown();
        } finally {
            shutdownClusterMemberGroups(memberGroups);
        }
    }

    /**
     * Launches a littlegrid cluster member group, this technique is useful when an external process
     * is required - for instance, if launching littlegrid from .Net or perhaps if you want to run
     * mini-clusters on your development machine and connect to then via an external process such as
     * WebLogic or Tomcat etc.
     *
     * @param args Arguments - this is expected to be the name of a single properties file from
     *             which the cluster member group configuration should be specified.
     * @throws IOException - exception.
     * @since 2.13
     */
    public static void main(final String[] args)
            throws IOException {

        final ClusterMemberGroup memberGroup = launchClusterMemberGroup(args);

        System.out.println();
        System.out.println("Cluster member group launched, press Enter to shutdown...");
        System.in.read();

        shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    static ClusterMemberGroup launchClusterMemberGroup(final String[] args) {
        if (args.length != 1) {
            System.out.println("Cannot launch cluster member group - please specify a properties file containing "
                    + "the configuration");

            System.exit(1);
        }

        System.setProperty(ClusterMemberGroup.Builder.BUILDER_OVERRIDE_KEY, args[0]);

        return newBuilder().buildAndConfigureForNoClient();
    }
}
