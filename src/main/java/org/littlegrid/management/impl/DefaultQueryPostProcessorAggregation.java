package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.aggregator.CompositeAggregator;
import org.littlegrid.management.TabularResultSet;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.util.Collections.singletonMap;

/**
 * Aggregation query post processor implementation.
 */
class DefaultQueryPostProcessorAggregation implements QueryPostProcessor {
    private final TabularResultSet results;

    /**
     * Constructor.
     *
     * @param queryResultsToProcess Results to process.
     * @param aggregation           Aggregation(s) to apply.
     * @param restriction           Restriction to apply.
     */
    @SuppressWarnings("unchecked")
    public DefaultQueryPostProcessorAggregation(final TabularResultSet queryResultsToProcess,
                                                final EntryAggregator aggregation,
                                                final Filter restriction) {

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction =
                QueryPostProcessorUtils.convertToEntries(queryResultsToProcess);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction, restriction);

        this.results = performAggregation(entriesAfterRestriction, aggregation);
    }

    @SuppressWarnings("unchecked")
    static TabularResultSet performAggregation(final Set<Map.Entry<Integer, Map<String, Object>>> entriesToAggregate,
                                               final EntryAggregator aggregation) {

        final TabularResultSet resultsToReturn = new DefaultTabularResultSet();

        final Object aggregationResult = aggregation.aggregate(entriesToAggregate);

        if (aggregationResult instanceof List) {
            resultsToReturn.addRow(createRowFromList(aggregation, aggregationResult));
        } else if (aggregationResult instanceof Map) {
            throw new UnsupportedOperationException();
        } else {
            resultsToReturn.addRow(singletonMap(aggregation.toString(), aggregationResult));
        }

        return resultsToReturn;
    }

    private static Map<String, Object> createRowFromList(final EntryAggregator aggregation,
                                                         final Object aggregationResult) {

        final List<Object> list = (List<Object>) aggregationResult;
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
    public TabularResultSet getResult() {
        return results;
    }
}
