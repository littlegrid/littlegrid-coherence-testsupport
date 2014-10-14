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
     * @return returns the number of rows in the result.
     */
    int getRowCount();

    /**
     * @return returns the number of columns in the result.
     */
    int getColumnCount();

    /**
     * @return returns the data within the result.
     */
    Collection<Map<String, Object>> getRows();
}
