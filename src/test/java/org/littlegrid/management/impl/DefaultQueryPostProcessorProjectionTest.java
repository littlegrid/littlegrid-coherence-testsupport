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
public class
        DefaultQueryPostProcessorProjectionTest extends AbstractQueryPostProcessorTest {
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
