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

import com.tangosol.util.aggregator.CompositeAggregator;
import com.tangosol.util.aggregator.Count;
import com.tangosol.util.aggregator.DoubleMax;
import com.tangosol.util.aggregator.DoubleMin;
import com.tangosol.util.aggregator.DoubleSum;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import org.littlegrid.management.TabularResult;

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

        final TabularResult results = DefaultQueryPostProcessorAggregation.performAggregation(
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
        final TabularResult results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), aggregator);

        assertThat(results.getRowCount(), is(1));

        final Map<String, Object> row = results.getRows().iterator().next();
        assertThat(row.get(aggregator.toString()), nullValue());
    }

    @Test
    public void aggregationForSeveralAggregatesWhenSeveralEntriesInSet() {
        final int numberOfEntries = 10;

        final TabularResult results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), AGGREGATION_WITH_SEVERAL_AGGREGATES);

        assertThat(results.getRowCount(), is(1));

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

        final TabularResult results = DefaultQueryPostProcessorAggregation.performAggregation(
                getPopulatedEntries(numberOfEntries), aggregator);

        assertThat(results.getRowCount(), is(1));

        final Collection<String> columnNames = results.getColumnNames();
        assertThat(columnNames.size(), is(1));

        final String columnName = aggregator.toString();
        assertThat(columnNames.contains(columnName), is(true));

        final Map<String, Object> row = results.getRows().iterator().next();
        assertThat(row.get(columnName), CoreMatchers.<Object>is(numberOfEntries));
    }
}
