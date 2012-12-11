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

import com.tangosol.coherence.dslquery.QueryPlus;
import com.tangosol.net.CacheFactory;

import java.io.IOException;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum;

/**
 * Cluster member group launcher.
 *
 * @since 2.14
 */
public class ClusterMemberGroupLauncher {
    /**
     * Launches a littlegrid cluster member group, this technique is useful when an external process
     * is required - for instance, if launching littlegrid from .Net or perhaps if you want to run
     * mini-clusters on your development machine and connect to then via an external process such as
     * WebLogic or Tomcat etc.
     *
     * @param args Arguments - this is expected to be the type of {@link BuildAndConfigureEnum} and
     *             optionally the name of a single properties file from which the cluster member group
     *             configuration could be specified.
     * @throws IOException - exception.
     */
    public static void main(final String[] args)
            throws Exception {

        final ClusterMemberGroup memberGroup = launchClusterMemberGroup(args);
        final BuildAndConfigureEnum buildAndConfigure = Enum.valueOf(BuildAndConfigureEnum.class, args[0]);

        if (buildAndConfigure == BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT) {
            CacheFactory.main(new String[]{});
        } else if (buildAndConfigure == BuildAndConfigureEnum.EXTEND_CLIENT) {
            QueryPlus.main(new String[]{});
        } else {
            System.out.println();
            System.out.println("Cluster member group launched, press Enter to shutdown or Ctrl+C to kill the process");
            System.in.read();
        }

        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    static ClusterMemberGroup launchClusterMemberGroup(final String[] args) {
        if (args.length != 1 && args.length != 2) {
            final StringBuilder sb = new StringBuilder();

            for (final BuildAndConfigureEnum buildAndConfigureEnum : BuildAndConfigureEnum.values()) {
                if (sb.length() == 0) {
                    sb.append(buildAndConfigureEnum);
                } else {
                    sb.append("|");
                    sb.append(buildAndConfigureEnum);
                }
            }

            final String className = ClusterMemberGroupLauncher.class.getName();

            System.out.println(format("Usage  : %s %s optionalPropertiesFile", className, sb));
            System.out.println();
            System.out.println(format("Example: %s STORAGE_DISABLED_CLIENT my-littlegrid.properties", className));

            System.exit(1);
        }

        final BuildAndConfigureEnum buildAndConfigure = Enum.valueOf(BuildAndConfigureEnum.class, args[0]);

        if (args.length == 2) {
            System.setProperty(ClusterMemberGroup.Builder.BUILDER_OVERRIDE_KEY, args[1]);
        }

        return ClusterMemberGroupUtils.newBuilder().buildAndConfigureFor(buildAndConfigure);
    }
}
