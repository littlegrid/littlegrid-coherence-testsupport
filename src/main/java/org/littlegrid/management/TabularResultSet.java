package org.littlegrid.management;

import java.util.Collection;
import java.util.Map;

/**
 * Query result.
 */
public interface TabularResultSet {
    void addRow(Map<String, Object> result);

    /**
     * @return returns the column names in the result.
     */
    Collection<String> getColumnNames();

    /**
     * @return returns the row count in the result.
     */
    int size();

    /**
     * @return returns the data within the result.
     */
    Collection<Map<String, Object>> getRows();
}
