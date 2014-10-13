package org.littlegrid.management.impl;

import com.tangosol.util.aggregator.CompositeAggregator;
import com.tangosol.util.aggregator.Count;
import com.tangosol.util.aggregator.DoubleMax;
import com.tangosol.util.aggregator.DoubleMin;
import com.tangosol.util.aggregator.DoubleSum;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import org.littlegrid.management.TabularResultSet;

import java.util.Collection;
import java.util.Map;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Query post-processor aggregation implementation tests.
 */
public class DefaultQueryPostProcessorAggregationTest extends AbstractQueryPostProcessorTest {
    private static final EntryAggregator MIN_AGE_AGGREGATOR = new DoubleMin(AGE_EXTRACTOR);
    private static final EntryAggregator MAX_AGE_AGGREGATOR = new DoubleMax(AGE_EXTRACTOR);
    private static final EntryAggregator AGGREGATION_WITH_SEVERAL_AGGREGATES =
            new CompositeAggregator().createInstance(new EntryAggregator[]{
                    MIN_AGE_AGGREGATOR,
                    MAX_AGE_AGGREGATOR});

    @Test
    public void aggregationWhenNoEntriesInSet() {
        aggregationForOneAggregateWhen(0);
    }

    @Test
    public void aggregationForOneAggregateWhenOneEntryInSet() {
        aggregationForOneAggregateWhen(1);
    }

    @Test
    public void aggregationForOneAggregateWhenSeveralEntriesInSet() {
        aggregationForOneAggregateWhen(25);
    }

    @Test
    public void aggregationForSeveralAggregatesWhenOneEntryInSet() {
        final int numberOfEntries = 1;

        final TabularResultSet results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), AGGREGATION_WITH_SEVERAL_AGGREGATES);

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(2));
        assertThat(columnNames.contains(MIN_AGE_AGGREGATOR.toString()), is(true));
        assertThat(columnNames.contains(MAX_AGE_AGGREGATOR.toString()), is(true));
    }

    @Test
    public void aggregationForNonExistentColumnWhenSeveralEntriesInSet() {
        final EntryAggregator aggregator = new DoubleSum(OTHER_EXTRACTOR);
        final int numberOfEntries = 2;
        final TabularResultSet results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), aggregator);

        assertThat(results.size(), is(1));

        final Map<String, Object> row = results.getRows().iterator().next();
        assertThat(row.get(aggregator.toString()), nullValue());
    }

    @Test
    public void aggregationForSeveralAggregatesWhenSeveralEntriesInSet() {
        final int numberOfEntries = 10;

        final TabularResultSet results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), AGGREGATION_WITH_SEVERAL_AGGREGATES);

        assertThat(results.size(), is(1));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(2));

        final String minColumnName = MIN_AGE_AGGREGATOR.toString();
        assertThat(columnNames.contains(minColumnName), is(true));

        final String maxColumnName = MAX_AGE_AGGREGATOR.toString();
        assertThat(columnNames.contains(maxColumnName), is(true));

        final Map<String, Object> row = results.getRows().iterator().next();

        assertThat(row.get(minColumnName), CoreMatchers.<Object>is(new Double(STARTING_AGE)));
        assertThat(row.get(maxColumnName), CoreMatchers.<Object>is(new Double(STARTING_AGE + (numberOfEntries - 1))));
    }

    private static void aggregationForOneAggregateWhen(final int numberOfEntries) {
        final EntryAggregator aggregator = new Count();

        final TabularResultSet results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), aggregator);

        assertThat(results.size(), is(1));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(1));

        final String columnName = aggregator.toString();
        assertThat(columnNames.contains(columnName), is(true));

        final Map<String, Object> row = results.getRows().iterator().next();
        assertThat(row.get(columnName), CoreMatchers.<Object>is(numberOfEntries));
    }
}
