package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import com.tangosol.util.aggregator.ReducerAggregator;
import com.tangosol.util.extractor.MultiExtractor;
import org.littlegrid.management.TabularResultSet;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.Collections.singletonMap;

/**
 * Projection query post processor implementation.
 */
class DefaultQueryPostProcessorProjection implements QueryPostProcessor {
    private final TabularResultSet results;

    /**
     * Constructor.
     *
     * @param queryResultsToProcess Results to process.
     * @param projection            Projection to apply.
     * @param restriction           Restriction to apply.
     */
    @SuppressWarnings("unchecked")
    public DefaultQueryPostProcessorProjection(final TabularResultSet queryResultsToProcess,
                                               final ValueExtractor projection,
                                               final Filter restriction) {

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction =
                QueryPostProcessorUtils.convertToEntries(queryResultsToProcess);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction, restriction);

        this.results = performProjection(entriesAfterRestriction, projection);
    }

    @SuppressWarnings("unchecked")
    static TabularResultSet performProjection(final Set<Map.Entry<Integer, Map<String, Object>>> entriesToRestrict,
                                              final ValueExtractor projection) {

        final Map<Integer, Object> projectionResult =
                (Map<Integer, Object>) new ReducerAggregator(projection).aggregate(entriesToRestrict);

        final TabularResultSet resultsToReturn = new DefaultTabularResultSet();

        for (final Object object : projectionResult.values()) {
            if (object instanceof List) {
                resultsToReturn.addRow(createRowFromList(projection, object));
            } else {
                resultsToReturn.addRow(singletonMap(projection.toString(), object));
            }
        }

        return resultsToReturn;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> createRowFromList(final ValueExtractor projection,
                                                         final Object object) {

        final List<Object> list = (List<Object>) object;
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
    public TabularResultSet getResult() {
        return results;
    }
}
