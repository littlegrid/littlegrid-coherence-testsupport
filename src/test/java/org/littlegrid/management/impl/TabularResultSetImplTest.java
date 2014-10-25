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

import org.hamcrest.CoreMatchers;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.management.TabularResultSet;

import java.util.Collection;
import java.util.Map;

import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tabular result implementation tests.
 */
public class TabularResultSetImplTest {
    @Test
    public void constructOnly() {
        final TabularResultSet result = new DefaultTabularResultSet();

        assertThat(result.getRowCount(), is(0));
        assertThat(result.getRows().size(), is(0));
        assertThat(result.getColumnCount(), is(0));
        assertThat(result.getColumnNames().size(), is(0));
    }

    @Test
    public void addRow() {
        final String key = "key";
        final Integer value = 123;

        final TabularResultSet result = new DefaultTabularResultSet();

        result.addRow(singletonMap(key, (Object) value));

        assertThat(result.getRowCount(), is(1));

        final Collection<Map<String, Object>> rows = result.getRows();
        assertThat(rows.size(), is(1));

        assertThat(result.getColumnCount(), is(1));

        final Collection<String> columns = result.getColumnNames();
        assertThat(columns.size(), is(1));
        assertThat(columns.iterator().next(), is(key));

        final Map<String, Object> row = rows.iterator().next();
        assertThat(row.get(key), CoreMatchers.<Object>is(value));
    }

    @Test
    @Ignore
    public void addNonUniformRow() {
    }
}
