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
import com.tangosol.util.extractor.ReflectionExtractor;
import org.littlegrid.management.TabularResult;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static java.util.Map.Entry;

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

    protected static Set<Entry<Integer, Map<String, Object>>> getPopulatedEntries(final int numberOfEntries,
                                                                                  final String... otherColumns) {

        return QueryPostProcessorUtils.convertToEntries(getResultsToUse(numberOfEntries, otherColumns));
    }

    private static TabularResult getResultsToUse(final int numberOfRows,
                                                    final String... otherColumns) {

        final TabularResult result = new DefaultTabularResult();

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
