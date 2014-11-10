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

import java.util.Collection;
import java.util.Map;

/**
 * Tabular result, contains the results of some query or processing.
 *
 * @since 2.16
 */
public interface TabularResult {
    /**
     * Adds a simple row containing just one column.
     *
     * @param key   Key.
     * @param value Value.
     */
    void addRow(String key,
                Object value);

    /**
     * Adds a row.
     *
     * @param row Row.
     */
    void addRow(Map<String, Object> row);

    /**
     * Adds all the rows.
     *
     * @param rows Rows.
     */
    void addRows(Collection<Map<String, Object>> rows);

    /**
     * @return the column names in the result.
     */
    Collection<String> getColumnNames();

    /**
     * @return if the specified column exists.
     */
    boolean containsColumn(String columnName);

    /**
     * @return if the specified row exists.
     */
    boolean containsRow(int rowNumber);

    /**
     * @return the number of rows in the result.
     */
    int getRowCount();

    /**
     * @return the number of columns in the result.
     */
    int getColumnCount();

    /**
     * @return the data within the result.
     */
    Collection<Map<String, Object>> getRows();

    /**
     * Gets a value for the specified column and row.
     *
     * @param columnName Column name.
     * @param rowNumber  Row number (starts from 0).
     * @return value.
     */
    Object getValue(String columnName,
                    int rowNumber);

    /**
     * Returns if contains the specified column and row.
     *
     * @param columnName Column name.
     * @param rowNumber  Row number (starts from 0).
     * @return <code>true</code> if exists.
     */
    boolean contains(String columnName,
                     int rowNumber);
}
