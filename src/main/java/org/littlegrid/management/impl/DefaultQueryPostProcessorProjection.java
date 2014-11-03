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
import com.tangosol.util.aggregator.ReducerAggregator;
import com.tangosol.util.extractor.MultiExtractor;
import org.littlegrid.management.TabularResult;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.Collections.singletonMap;
import static java.util.Map.Entry;

/**
 * Projection query post processor implementation.
 *
 * @since 2.16
 */
class DefaultQueryPostProcessorProjection implements QueryPostProcessor {
    private final TabularResult results;

    /**
     * Constructor.
     *
     * @param queryResultsToProcess Results to process.
     * @param projection            Projection to apply.
     * @param restriction           Restriction to apply.
     */
    @SuppressWarnings("unchecked")
    public DefaultQueryPostProcessorProjection(final TabularResult queryResultsToProcess,
                                               final ValueExtractor projection,
                                               final Filter restriction) {

        final Set<Entry<Integer, Map<String, Object>>> entriesBeforeRestriction =
                QueryPostProcessorUtils.convertToEntries(queryResultsToProcess);

        final Set<Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction, restriction);

        this.results = performProjection(entriesAfterRestriction, projection);
    }

    @SuppressWarnings("unchecked")
    static TabularResult performProjection(final Set<Entry<Integer, Map<String, Object>>> entriesToRestrict,
                                              final ValueExtractor projection) {

        final Map<Integer, Object> projectionResult =
                (Map<Integer, Object>) new ReducerAggregator(projection).aggregate(entriesToRestrict);

        final TabularResult resultsToReturn = new DefaultTabularResult();

        for (final Object object : projectionResult.values()) {
            if (object instanceof List) {
                resultsToReturn.addRow(createRowFromList(projection, (List<Object>) object));
            } else {
                resultsToReturn.addRow(singletonMap(projection.toString(), object));
            }
        }

        return resultsToReturn;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> createRowFromList(final ValueExtractor projection,
                                                         final List<Object> list) {

        final MultiExtractor multiExtractor = (MultiExtractor) projection;
        final ValueExtractor[] extractors = multiExtractor.getExtractors();
        final int numberOfExtractors = extractors.length;
        final Map<String, Object> row = new LinkedHashMap<String, Object>(numberOfExtractors);

        for (int i = 0; i < numberOfExtractors; i++) {
            row.put(extractors[i].toString(), list.get(i));
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
