package org.littlegrid.management.impl;

import com.tangosol.util.ValueExtractor;
import com.tangosol.util.extractor.MultiExtractor;
import com.tangosol.util.extractor.ReflectionExtractor;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import org.littlegrid.management.TabularResultSet;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Query post-processor projection implementation tests.
 */
public class DefaultQueryPostProcessorProjectionTest extends AbstractQueryPostProcessorTest {
    private static final ValueExtractor PROJECTION_WITH_SEVERAL_COLUMNS = new MultiExtractor(new ValueExtractor[]{
            AGE_EXTRACTOR,
            OTHER_EXTRACTOR});

    @Test
    public void projectionWhenNoEntriesInSet() {
        final TabularResultSet results = DefaultQueryPostProcessorProjection.performProjection(
                new HashSet<Map.Entry<Integer, Map<String, Object>>>(), AGE_EXTRACTOR);

        assertThat(results.getRowCount(), is(0));
    }

    @Test
    public void projectionForOneColumnWhenOneEntryInSet() {
        final int numberOfEntries = 1;

        final TabularResultSet results = DefaultQueryPostProcessorProjection.performProjection(
                getPopulatedEntries(numberOfEntries),
                AGE_EXTRACTOR);

        assertThat(results.getRowCount(), is(numberOfEntries));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(1));

        final String columnName = AGE_EXTRACTOR.toString();
        assertThat(columnNames.contains(columnName), is(true));

        final Map<String, Object> row = results.getRows().iterator().next();
        assertThat(row.get(columnName), CoreMatchers.<Object>is(STARTING_AGE));
    }

    @Test
    public void projectionForSeveralColumnsWhenOneEntryInSet() {
        final int numberOfEntries = 1;

        final TabularResultSet results = DefaultQueryPostProcessorProjection.performProjection(
                getPopulatedEntries(numberOfEntries, OTHER_COLUMN),
                PROJECTION_WITH_SEVERAL_COLUMNS);

        assertThat(results.getRowCount(), is(numberOfEntries));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(2));
        assertThat(columnNames.contains(AGE_EXTRACTOR.toString()), is(true));
        assertThat(columnNames.contains(OTHER_EXTRACTOR.toString()), is(true));
    }

    @Test
    public void projectionForNonExistentColumnWhenSeveralEntriesInSet() {
        final int numberOfEntries = 2;
        final ValueExtractor extractorForColumnNotInRow = new ReflectionExtractor("get", new Object[]{"Name"});

        final TabularResultSet results = DefaultQueryPostProcessorProjection.performProjection(
                getPopulatedEntries(numberOfEntries, OTHER_COLUMN),
                extractorForColumnNotInRow);

        assertThat(results.getRowCount(), is(numberOfEntries));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(1));
        assertThat(columnNames.contains(extractorForColumnNotInRow.toString()), is(true));
    }

    @Test
    public void projectionForSeveralColumnsWhenSeveralEntriesInSet() {
        final int numberOfEntries = 2;

        final TabularResultSet results = DefaultQueryPostProcessorProjection.performProjection(
                getPopulatedEntries(numberOfEntries, OTHER_COLUMN),
                PROJECTION_WITH_SEVERAL_COLUMNS);

        assertThat(results.getRowCount(), is(numberOfEntries));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(2));
        assertThat(columnNames.contains(AGE_EXTRACTOR.toString()), is(true));
        assertThat(columnNames.contains(OTHER_EXTRACTOR.toString()), is(true));
    }
}
