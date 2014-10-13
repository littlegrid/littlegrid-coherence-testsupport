package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.SimpleMapEntry;
import org.littlegrid.management.TabularResultSet;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Query post processor utilities.
 */
class QueryPostProcessorUtils {
    @SuppressWarnings("unchecked")
    static Set<Map.Entry<Integer, Map<String, Object>>> convertToEntries(final TabularResultSet results) {
        final Set<Map.Entry<Integer, Map<String, Object>>> entries =
                new HashSet<Map.Entry<Integer, Map<String, Object>>>(results.size());

        int counter = 1;

        for (final Map<String, Object> row : results.getRows()) {
            entries.add(new SimpleMapEntry(counter, row));

            counter++;
        }

        return entries;
    }

    static Set<Map.Entry<Integer, Map<String, Object>>> performRestriction(
            final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction,
            final Filter restriction) {

        final Set<Map.Entry<Integer, Map<String, Object>>> results =
                new HashSet<Map.Entry<Integer, Map<String, Object>>>();

        for (final Map.Entry<Integer, Map<String, Object>> entry : entriesBeforeRestriction) {
            final Object value = entry.getValue();

            if (restriction.evaluate(value)) {
                results.add(entry);
            }
        }

        return results;
    }

//    static Object
}
