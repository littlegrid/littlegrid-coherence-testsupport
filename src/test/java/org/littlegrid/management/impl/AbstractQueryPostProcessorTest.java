package org.littlegrid.management.impl;

import com.tangosol.util.ValueExtractor;
import com.tangosol.util.extractor.ReflectionExtractor;
import org.littlegrid.management.TabularResultSet;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Abstract query post-processor tests base class.
 */
public abstract class AbstractQueryPostProcessorTest {
    protected static final String OTHER_COLUMN = "ColumnA";
    protected static final ValueExtractor OTHER_EXTRACTOR = new ReflectionExtractor("get", new Object[]{OTHER_COLUMN});

    protected static final int STARTING_AGE = 10;
    protected static final String AGE_COLUMN_NAME = "age";
    protected static final ValueExtractor AGE_EXTRACTOR =
            new ReflectionExtractor("get", new Object[]{AGE_COLUMN_NAME});

    protected static Set<Map.Entry<Integer, Map<String, Object>>> getPopulatedEntries(final int numberOfEntries,
                                                                                      final String... otherColumns) {

        return QueryPostProcessorUtils.convertToEntries(getResultsToUse(numberOfEntries, otherColumns));
    }

    private static TabularResultSet getResultsToUse(final int numberOfRows,
                                                    final String... otherColumns) {

        final TabularResultSet result = new DefaultTabularResultSet();

        for (int i = 0; i < numberOfRows; i++) {
            final Map<String, Object> row = new HashMap<String, Object>();
            row.put(AGE_COLUMN_NAME, STARTING_AGE + i);

            for (final String otherColumn : otherColumns) {
                row.put(otherColumn, 1000 + i);
            }

            result.addRow(row);
        }

        return result;
    }
}
