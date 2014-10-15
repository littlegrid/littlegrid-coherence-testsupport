package org.littlegrid.management.impl;

import org.littlegrid.management.TabularResultSet;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

/**
 * Tabular result implementation.
 */
//TODO: look to reduce scope again??
public class DefaultTabularResultSet implements TabularResultSet {
    private final Collection<Map<String, Object>> rows = new ArrayList<Map<String, Object>>();
    private final Collection<String> columns = new HashSet<String>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void addRow(final Map<String, Object> row) {
        //TODO: add check to ensure row being added is uniform with the existing rows.

        columns.addAll(row.keySet());
        rows.add(row);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<String> getColumnNames() {
        return new HashSet<String>(columns);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRowCount() {
        return rows.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getColumnCount() {
        return columns.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<Map<String, Object>> getRows() {
        return new ArrayList<Map<String, Object>>(rows);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();

        for (final Map row : rows) {
            sb.append(row);
            sb.append("\n");
        }

        return sb.toString();
    }
}
