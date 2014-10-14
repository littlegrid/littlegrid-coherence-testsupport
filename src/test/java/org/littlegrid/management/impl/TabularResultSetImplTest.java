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
