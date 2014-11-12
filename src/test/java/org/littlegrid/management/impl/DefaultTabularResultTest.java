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

import org.hamcrest.CoreMatchers;
import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.management.ManagementIdentifiableException;
import org.littlegrid.management.TabularResult;

import java.util.Collection;
import java.util.Map;

import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.littlegrid.management.ManagementIdentifiableException.ReasonEnum.INVALID_COLUMN_NAME;
import static org.littlegrid.management.ManagementIdentifiableException.ReasonEnum.INVALID_ROW_NUMBER;

/**
 * Default tabular result tests.
 */
public class DefaultTabularResultTest {
    private static final String KNOWN_KEY = "some-key";
    private static final String KNOWN_VALUE = "some-value";

    //TODO: include tests for add row.

    @Test
    public void constructOnly() {
        final TabularResult result = new DefaultTabularResult();

        assertThat(result.getRowCount(), is(0));
        assertThat(result.getRows().size(), is(0));
        assertThat(result.getColumnCount(), is(0));
        assertThat(result.getColumnNames().size(), is(0));
    }

    @Test
    public void addKeyValueRow() {
        final String key = "key";
        final Integer value = 123;

        final TabularResult result = new DefaultTabularResult();

        result.addRow(key, value);

        checkResultAsExpected(result, key, value);
    }

    private static void checkResultAsExpected(TabularResult result, String key, Integer value) {
        assertThat(result.getRowCount(), is(1));

        final Collection<Map<String, Object>> rows = result.getRows();
        assertThat(rows.size(), is(1));

        assertThat(result.getColumnCount(), is(1));

        final Collection<String> columns = result.getColumnNames();
        assertThat(columns.size(), is(1));
        assertThat(columns.iterator().next(), is(key));

        final Map<String, Object> row = rows.iterator().next();
        assertThat(row.get(key), CoreMatchers.<Object>is(value));
    }

    @Test
    public void addRow() {
        final String key = "key";
        final Integer value = 123;

        final TabularResult result = new DefaultTabularResult();

        result.addRow(singletonMap(key, (Object) value));

        checkResultAsExpected(result, key, value);
    }

    @Test
    @Ignore
    public void addNonUniformRow() {
    }

    @Test
    public void containsColumnWhenColumnDoesNotExist() {
        final TabularResult result = getPopulatedResult();

        final boolean exists = result.containsColumn("unknown-key");
        assertThat(exists, is(false));
    }

    @Test
    public void containsRowWhenRowDoesNotExist() {
        final TabularResult result = getPopulatedResult();

        final boolean exists = result.containsRow(result.getRowCount() + 10);
        assertThat(exists, is(false));
    }

    @Test
    public void containsRowWhenNegativeRowThatDoesNotExist() {
        final TabularResult result = getPopulatedResult();

        final boolean exists = result.containsRow(-10);
        assertThat(exists, is(false));
    }

    @Test
    public void containsRowWhenZeroRowThatDoesNotExist() {
        final TabularResult result = new DefaultTabularResult();

        final boolean exists = result.containsRow(0);
        assertThat(exists, is(false));
    }

    @Test
    public void existsWhenColumnAndRowDoExist() {
        final TabularResult result = getPopulatedResult();

        final boolean exists = result.contains(KNOWN_KEY, result.getRowCount() - 1);
        assertThat(exists, is(true));
    }

    @Test
    public void getValueForColumnThatDoesNotExist() {
        final TabularResult result = getPopulatedResult();

        try {
            result.getValue("unknown-key", 0);

            fail("Exception expected");
        } catch (ManagementIdentifiableException e) {
            assertThat(e.getReasonEnum(), is(INVALID_COLUMN_NAME));
        } catch (Exception e) {
            fail("Wrong type of exception: " + e);
        }
    }

    @Test
    public void getValueForRowThatDoesNotExist() {
        final TabularResult result = getPopulatedResult();

        try {
            result.getValue(KNOWN_KEY, result.getRowCount() + 10);

            fail("Exception expected");
        } catch (ManagementIdentifiableException e) {
            assertThat(e.getReasonEnum(), is(INVALID_ROW_NUMBER));
        } catch (Exception e) {
            fail("Wrong type of exception: " + e);
        }
    }

    @Test
    public void getValueForColumnAndRowThatExist() {
        final TabularResult result = getPopulatedResult();

        final Object value = result.getValue(KNOWN_KEY, result.getRowCount() - 1);
        assertThat((String) value, is(KNOWN_VALUE));
    }

    private static TabularResult getPopulatedResult() {
        final TabularResult result = new DefaultTabularResult();
        result.addRow(KNOWN_KEY, KNOWN_VALUE);

        return result;
    }
}
