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

package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import static com.tangosol.util.InvocableMap.EntryAggregator;

/**
 * Management repository.
 *
 * @since 2.16
 */
interface ManagementRepository {
    /**
     * Finds management information and performs a projection (values from
     * underlying target) based on restriction.
     *
     * @param projection  Projection - (can be a composite) values from target.
     * @param queryTarget Target of query.
     * @param restriction Restriction.
     * @return query results.
     */
    TabularResultSet findManagementInformationByCriteria(ValueExtractor projection,
                                                         String queryTarget,
                                                         Filter restriction);

    /**
     * Finds management information and performs a projection (values from
     * underlying target) based on restriction.
     *
     * @param aggregation Aggregation - (can be a composite) such as sum/average from target.
     * @param queryTarget Target of query.
     * @param restriction Restriction.
     * @return query results.
     */
    TabularResultSet findManagementInformationByCriteria(EntryAggregator aggregation,
                                                         String queryTarget,
                                                         Filter restriction);

    TabularResultSet createManagementInformationSnapshot(String snapshotName,
                                                         String snapshotQuery);

    boolean dropManagementInformationSnapshot(String snapshotName);

    TabularResultSet findSnapshots();

    TabularResultSet describeSnapshot(String snapshotName);
}
