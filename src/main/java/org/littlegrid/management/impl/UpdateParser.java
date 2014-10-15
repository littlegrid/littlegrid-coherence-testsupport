package org.littlegrid.management.impl;

import com.tangosol.util.Filter;

/**
 * Update parser.
 */
public interface UpdateParser {
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
}
