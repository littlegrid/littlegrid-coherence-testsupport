package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import java.util.Collection;

import static com.tangosol.util.InvocableMap.EntryAggregator;

/**
 * Management repository.
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

    int createManagementInformationSnapshot(String snapshotName,
                                            String snapshotQuery);

    boolean dropManagementInformationSnapshot(String snapshotName);

    Collection<String> findSnapshots();
}
