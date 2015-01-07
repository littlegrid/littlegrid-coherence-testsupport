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
import com.tangosol.util.SimpleMapEntry;
import org.littlegrid.management.TabularResult;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static java.util.Map.Entry;

/**
 * Query post processor utilities.
 *
 * @since 2.16
 */
class QueryPostProcessorUtils {
    /**
     * Converts tabular results to set of entries.
     *
     * @param results Tabular results.
     * @return entries.
     */
    @SuppressWarnings("unchecked")
    static Set<Entry<Integer, Map<String, Object>>> convertToEntries(final TabularResult results) {
        final Set<Entry<Integer, Map<String, Object>>> entries =
                new HashSet<Entry<Integer, Map<String, Object>>>(results.getRowCount());

        int counter = 1;

        for (final Map<String, Object> row : results.getRows()) {
            entries.add(new SimpleMapEntry(counter, row));

            counter++;
        }

        return entries;
    }

    /**
     * Performs a restriction on a set of entries.
     *
     * @param entriesBeforeRestriction Set of entries before restriction.
     * @param restriction              Restriction to apply.
     * @return entries after restriction.
     */
    static Set<Entry<Integer, Map<String, Object>>> performRestriction(
            final Set<Entry<Integer, Map<String, Object>>> entriesBeforeRestriction,
            final Filter restriction) {

        final Set<Entry<Integer, Map<String, Object>>> results =
                new HashSet<Entry<Integer, Map<String, Object>>>();

        for (final Entry<Integer, Map<String, Object>> entry : entriesBeforeRestriction) {
            final Object value = entry.getValue();

            if (restriction.evaluate(value)) {
                results.add(entry);
            }
        }

        return results;
    }
}
