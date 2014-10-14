package org.littlegrid.management.impl;

import com.tangosol.util.filter.AlwaysFilter;
import com.tangosol.util.filter.GreaterEqualsFilter;
import com.tangosol.util.filter.LessFilter;
import org.hamcrest.CoreMatchers;
import org.junit.Test;
import org.littlegrid.management.TabularResultSet;

import java.util.Map;
import java.util.Set;

import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Query post-processor utilities tests.
 */
public class QueryPostProcessorUtilsTest extends AbstractQueryPostProcessorTest {
    @Test
    public void convertWhenResultsHasNoRows() {
        final Set<Map.Entry<Integer, Map<String, Object>>> entries =
                QueryPostProcessorUtils.convertToEntries(new DefaultTabularResultSet());

        assertThat(entries.size(), is(0));
    }

    @Test
    public void convertWhenResultsHasRow() {
        final String key = "key";
        final String value = "value";

        final TabularResultSet results = new DefaultTabularResultSet();
        results.addRow(singletonMap(key, (Object) value));

        final Set<Map.Entry<Integer, Map<String, Object>>> entries =
                QueryPostProcessorUtils.convertToEntries(results);

        assertThat(entries.size(), is(results.getRowCount()));

        final Map.Entry<Integer, Map<String, Object>> entry = entries.iterator().next();
        assertThat(entry.getKey(), is(1));

        final Map<String, Object> map = entry.getValue();
        assertThat(map.size(), is(1));
        assertThat(map.get(key), CoreMatchers.<Object>is(value));
    }

    @Test
    public void restrictionWhenNoEntriesInSet() {
        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction = getPopulatedEntries(0);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction,
                        AlwaysFilter.INSTANCE);

        assertThat(entriesAfterRestriction.size(), is(0));
    }

    @Test
    public void restrictionThatMatchesAllWhenOneEntryInSet() {
        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction = getPopulatedEntries(1);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction,
                        AlwaysFilter.INSTANCE);

        assertThat(entriesAfterRestriction.size(), is(1));
    }

    @Test
    public void restrictionThatMatchesNoneWhenOneEntryInSet() {
        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction = getPopulatedEntries(1);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction,
                        new GreaterEqualsFilter(AGE_EXTRACTOR, 40));

        assertThat(entriesAfterRestriction.size(), is(0));
    }

    @Test
    public void restrictionThatMatchesSomeWhenSeveralEntriesInSet() {
        final Set<Map.Entry<Integer, Map<String, Object>>> entriesBeforeRestriction = getPopulatedEntries(10);

        final Set<Map.Entry<Integer, Map<String, Object>>> entriesAfterRestriction =
                QueryPostProcessorUtils.performRestriction(entriesBeforeRestriction,
                        new LessFilter(AGE_EXTRACTOR, STARTING_AGE + 5));

        assertThat(entriesAfterRestriction.size(), is(5));
    }
}
