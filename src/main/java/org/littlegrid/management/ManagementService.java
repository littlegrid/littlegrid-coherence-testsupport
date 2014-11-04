/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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

package org.littlegrid.management;

import javax.management.MBeanServerConnection;

/**
 * Management service.
 *
 * @since 2.16
 */
public interface ManagementService {
    /**
     * Returns the prefix that denotes an alias, an alias being a short-cut for a longer
     * query.
     *
     * @return alias.
     */
    String getAliasPrefix();

    /**
     * Finds management information based upon the supplied query.
     *
     * @param query Query.
     * @return results.
     */
    TabularResult findManagementInformation(String query);

    /**
     * Creates a snapshot using the supplied query.
     *
     * @param name  Snapshot name (may omit the snapshot prefix as it will be added).
     * @param query Query that should be used to create snapshot.
     * @return summary information.
     */
    TabularResult createSnapshot(String name,
                                 String query);

    /**
     * Drops a snapshot.
     *
     * @param name Snapshot name.
     * @return <code>true</code> if dropped.
     */
    boolean dropSnapshot(String name);

    /**
     * Finds summary information about the existing snapshots.
     *
     * @return summary information.
     */
    TabularResult findSnapshotSummaries();

    /**
     * Describes a particular snapshot in detail.
     *
     * @param name Snapshot name.
     * @return snapshot detailed information.
     */
    TabularResult describeSnapshot(String name);


    /**
     * Management service builder.
     */
    interface Builder {
        String BUILDER_SYSTEM_PROPERTY_PREFIX_KEY = "littlegrid.management.builder.";

        String BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY = "littlegrid_management_builder_";

        Builder setAliases(String commaDelimitedPropertiesFilenames);

        Builder setUrlPath(String urlPath);

        Builder setUsername(String username);

        Builder setPassword(String password);

        Builder setMBeanServerConnection(MBeanServerConnection mBeanServerConnection);

        Builder setAliasPrefix(String aliasPrefix);

        Builder setSnapshotPrefix(String snapshotPrefix);

        Builder setAliasValueDelimiter(String aliasValueDelimiter);

        ManagementService buildAndConnect();
    }
}
