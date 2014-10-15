package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;

import static com.tangosol.util.InvocableMap.EntryAggregator;

/**
 * Management query parser.
 */
interface QueryParser {
    /**
     * Returns the projection.
     *
     * @return <code>null</code> if no projection.
     */
    ValueExtractor getProjection();

    /**
     * Returns the target of the query.
     *
     * @return target.
     */
    String getTarget();

    /**
     * Returns the restriction.
     *
     * @return restriction.
     */
    Filter getRestriction();

    /**
     * Returns if an aggregation (basically not a projection).
     *
     * @return <code>true</code> if aggregation.
     */
    boolean isAggregation();

    /**
     * Returns aggregation.
     *
     * @return <code>null</code> if no aggregation.
     */
    EntryAggregator getAggregation();
}
