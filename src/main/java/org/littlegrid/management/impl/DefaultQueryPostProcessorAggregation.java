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

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import com.tangosol.util.aggregator.CompositeAggregator;
import com.tangosol.util.aggregator.GroupAggregator;
import com.tangosol.util.extractor.MultiExtractor;
import org.littlegrid.management.TabularResult;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.util.Collections.singletonMap;
import static java.util.Map.Entry;

/**
 * Aggregation query post processor implementation.
 *
 * @since 2.16
 */
class DefaultQueryPostProcessorAggregation implements QueryPostProcessor {
    private final TabularResult results;

    /**
     * Constructor.
     *
     * @param queryResultsToProcess Results to process.
     * @param aggregation           Aggregation(s) to apply.
     * @param restriction           Restriction to apply.
     */
    @SuppressWarnings("unchecked")
    public DefaultQueryPostProcessorAggregation(final TabularResult queryResultsToProcess,
                                                final EntryAggregator aggregation,
                                                final Filter restriction) {

        final Set<Entry<Integer, Map<String, Object>>> entriesBeforeRestriction =
                QueryPostProcessorUtils.convertToEntries(queryResultsToProcess);

        final Set<Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction, restriction);

        this.results = performAggregation(entriesAfterRestriction, aggregation);
    }

    @SuppressWarnings("unchecked")
    static TabularResult performAggregation(final Set<Entry<Integer, Map<String, Object>>> entriesToAggregate,
                                            final EntryAggregator aggregation) {

        final TabularResult resultsToReturn = new DefaultTabularResult();

        final Object aggregationResult = aggregation.aggregate(entriesToAggregate);

        if (aggregationResult instanceof List) {
            resultsToReturn.addRow(createRowFromList(aggregation, (List<Object>) aggregationResult));
        } else if (aggregationResult instanceof Map) {
            //TODO: add some tests
            resultsToReturn.addRows(createRowsFromMap(aggregation, (Map<Object, Object>) aggregationResult));
        } else {
            resultsToReturn.addRow(singletonMap(aggregation.toString(), aggregationResult));
        }

        return resultsToReturn;
    }

    private static Collection<Map<String, Object>> createRowsFromMap(final EntryAggregator aggregation,
                                                                     final Map<Object, Object> map) {

        final GroupAggregator groupAggregator = (GroupAggregator) aggregation;
        final ValueExtractor aggregationExtractor = groupAggregator.getExtractor();
        final ValueExtractor[] extractorColumns;

        if (aggregationExtractor instanceof MultiExtractor) {
            final MultiExtractor multiExtractor = (MultiExtractor) aggregationExtractor;

            extractorColumns = multiExtractor.getExtractors();
        } else {
            extractorColumns = new ValueExtractor[]{aggregationExtractor};
        }

        final EntryAggregator aggregationAggregator = groupAggregator.getAggregator();
        final EntryAggregator[] aggregatorColumns;

        if (aggregationAggregator instanceof CompositeAggregator) {
            final CompositeAggregator compositeAggregator = (CompositeAggregator) aggregationAggregator;

            aggregatorColumns = compositeAggregator.getAggregators();
        } else {
            aggregatorColumns = new EntryAggregator[]{aggregationAggregator};
        }

        final List<Object> columns = new ArrayList<Object>(extractorColumns.length + aggregatorColumns.length);
        Collections.addAll(columns, extractorColumns);
        Collections.addAll(columns, aggregatorColumns);

        final Collection<Map<String, Object>> rows = new ArrayList<Map<String, Object>>();

        for (final Entry entry : map.entrySet()) {
            final Map<String, Object> row = new LinkedHashMap<String, Object>();
            final List<Object> values = new ArrayList<Object>(columns.size());

            final Object key = entry.getKey();

            if (key instanceof Collection) {
                final Collection collection = (Collection) key;

                values.addAll(collection);
            } else {
                values.add(key);
            }

            final Object value = entry.getValue();

            if (value instanceof Collection) {
                final Collection collection = (Collection) value;

                values.addAll(collection);
            } else {
                values.add(value);
            }

            for (int i = 0; i < columns.size(); i++) {
                row.put(columns.get(i).toString(), values.get(i));
            }

            rows.add(row);
        }

        return rows;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> createRowFromList(final EntryAggregator aggregation,
                                                         final List<Object> list) {

        final CompositeAggregator compositeAggregator = (CompositeAggregator) aggregation;
        final EntryAggregator[] aggregators = compositeAggregator.getAggregators();
        final int numberOfAggregators = aggregators.length;
        final Map<String, Object> row = new LinkedHashMap<String, Object>(numberOfAggregators);

        for (int i = 0; i < numberOfAggregators; i++) {
            row.put(aggregators[i].toString(), list.get(i));
        }

        return row;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResult getResult() {
        return results;
    }
}
