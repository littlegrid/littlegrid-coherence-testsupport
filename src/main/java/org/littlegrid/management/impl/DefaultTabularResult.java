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

import org.littlegrid.management.ManagementIdentifiableException;
import org.littlegrid.management.TabularResult;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static org.littlegrid.management.ManagementIdentifiableException.ReasonEnum.INVALID_COLUMN_NAME;
import static org.littlegrid.management.ManagementIdentifiableException.ReasonEnum.INVALID_ROW_NUMBER;

/**
 * Tabular result set default implementation.
 *
 * @since 2.16
 */
class DefaultTabularResult implements TabularResult {
    private final List<Map<String, Object>> rows = new ArrayList<Map<String, Object>>();
    private final Collection<String> columns = new HashSet<String>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void addRow(final String key,
                       final Object value) {
        addRow(Collections.singletonMap(key, value));
    }

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
    public void addRows(final Collection<Map<String, Object>> rows) {
        for (final Map<String, Object> row : rows) {
            addRow(row);
        }
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
    public boolean containsColumn(final String columnName) {
        return columns.contains(columnName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsRow(final int rowNumber) {
        return !(rowNumber < 0 || rowNumber >= rows.size());
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
    public Object getValue(final String columnName,
                           final int rowNumber) {

        if (!containsColumn(columnName)) {
            throw new ManagementIdentifiableException(format("Column '%s' does not exist within %s",
                    columnName, columns),
                    INVALID_COLUMN_NAME);
        }

        if (!containsRow(rowNumber)) {
            throw new ManagementIdentifiableException(format("Row %d does not exist, there are %d rows",
                    rowNumber, rows.size()), INVALID_ROW_NUMBER);
        }

        final Map<String, Object> row = rows.get(rowNumber);

        return row.get(columnName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(final String columnName,
                            final int rowNumber) {

        return containsColumn(columnName) && containsRow(rowNumber);
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
