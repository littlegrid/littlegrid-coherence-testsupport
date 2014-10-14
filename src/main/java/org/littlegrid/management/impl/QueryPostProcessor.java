package org.littlegrid.management.impl;

import org.littlegrid.management.TabularResultSet;

/**
 * Query post processor.
 */
interface QueryPostProcessor {
    /**
     * Returns result of any post-processing.
     *
     * @return results.
     */
    TabularResultSet getResult();
}
