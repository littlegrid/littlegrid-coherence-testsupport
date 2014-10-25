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
