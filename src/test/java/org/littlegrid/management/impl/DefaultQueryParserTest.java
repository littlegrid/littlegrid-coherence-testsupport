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
import com.tangosol.util.aggregator.DoubleSum;
import com.tangosol.util.extractor.IdentityExtractor;
import com.tangosol.util.extractor.MultiExtractor;
import com.tangosol.util.extractor.ReflectionExtractor;
import com.tangosol.util.filter.AlwaysFilter;
import com.tangosol.util.filter.EqualsFilter;
import org.hamcrest.core.Is;
import org.junit.Ignore;
import org.junit.Test;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.management.impl.DefaultQueryParser.ATTRIBUTE_NAME_DEFAULT_INDICATOR;
import static org.littlegrid.management.impl.DefaultQueryParser.ATTRIBUTE_NAME_DEFAULT_PATTERN;

/**
 * Management query parser implementation tests.
 */
public class DefaultQueryParserTest {
    @Test
    public void ensureFromWhenPresent() {
        final String expected = " from test";
        final String actual = DefaultQueryParser.ensureFromIsPresent(expected);

        assertThat(actual, is(expected));
    }

    @Test
    public void ensureFromWhenNotPresent() {
        final String query = "test";
        final String expected = format(" from %s", query);

        final String actual = DefaultQueryParser.ensureFromIsPresent(query);

        assertThat(actual, is(expected));
    }

    @Test
    public void ensureSelectWhenPresent() {
        final String expected = "select x from test";
        final String actual = DefaultQueryParser.ensureSelectIsPresent(expected);

        assertThat(actual, is(expected));
    }

    @Test
    public void ensureSelectWhenNotPresent() {
        final String query = "from test";
        final String expected = format("select value() %s", query);

        final String actual = DefaultQueryParser.ensureSelectIsPresent(query);

        assertThat(actual, is(expected));
    }

    @Test(expected = IllegalArgumentException.class)
    public void ensureQuotedWhenNoFromAndSpace() {
        DefaultQueryParser.ensureFromTargetHasQuotes("select x from");
    }

    @Test(expected = IllegalArgumentException.class)
    public void ensureQuotedWhenFromAndSpaceButNoTarget() {
        DefaultQueryParser.ensureFromTargetHasQuotes("select x from ");
    }

    @Test(expected = IllegalArgumentException.class)
    public void ensureQuotedWhenNoFromAndSpaceWithTarget() {
        DefaultQueryParser.ensureFromTargetHasQuotes("select x fromy");
    }

    @Test
    public void simpleEnsureQuoted() {
        final String projection = "select x";
        final String target = "test";
        final String postTarget = "";

        ensureQuoted(projection, target, target, postTarget);
    }

    @Test
    public void ensureQuotedWhenLotsOfSpaces() {
        final String projection = "select x    ";
        final String target = "test";
        final String postTarget = "";

        ensureQuoted(projection, target, "  " + target + "  ", postTarget);
    }

    @Test
    public void ensureQuotedWhenWherePresent() {
        final String projection = "select x";
        final String target = "test";
        final String postTarget = " where testId = 1";

        ensureQuoted(projection, target, target, postTarget);
    }

    private void ensureQuoted(final String projection,
                              final String target,
                              final String targetWithAnySpaces,
                              final String postTarget) {

        final String query = format("%s from %s%s", projection, targetWithAnySpaces, postTarget);
        final String expected = format("%s from '%s'%s", projection, target, postTarget);

        final String actual = DefaultQueryParser.ensureFromTargetHasQuotes(query);

        assertThat(actual, is(expected));
    }

    @Test
    public void ensureQuotedWhenGroupByPresent() {
        final String projection = "select x";
        final String postTarget = " group by x";
        final String target = "test";
        final String query = format("%s from %s%s", projection, target, postTarget);
        final String expected = format("%s from '%s'%s", projection, target, postTarget);

        final String actual = DefaultQueryParser.ensureFromTargetHasQuotes(query);

        assertThat(actual, is(expected));
    }

    @Test
    public void ensureGettersWhenNoAttributeNameIndicatorsPresent() {
        final String expected = "select a, b, c from d where e = 'a' group by f";

        final String result = DefaultQueryParser.ensureAttributesConvertedToMapGets(
                ATTRIBUTE_NAME_DEFAULT_PATTERN, ATTRIBUTE_NAME_DEFAULT_INDICATOR, expected);

        assertThat(result, is(expected));
    }

    @Test
    public void ensureGettersWhenAttributeNameIndicatorsPresent() {
        final String starting = "select @a, @b, @c from d where @eeee = 'a' group by @f";
        final String expected = "select get('a'), get('b'), get('c') from d where get('eeee') = 'a' group by get('f')";

        final String result = DefaultQueryParser.ensureAttributesConvertedToMapGets(
                ATTRIBUTE_NAME_DEFAULT_PATTERN, ATTRIBUTE_NAME_DEFAULT_INDICATOR, starting);

        assertThat(result, is(expected));
    }

    @Test(expected = IllegalArgumentException.class)
    public void invalidSelect() {
        new DefaultQueryParser("x select from test");
    }

    @Test(expected = UnsupportedOperationException.class)
    @Ignore
    public void selectContainingSnapshotIndicator() {
        new DefaultQueryParser("select from ~test");
    }

    @Test
    public void simpleSelect() {
        final String target = "test";

        simpleSelect(target, target);
    }

    @Test
    public void simpleSelectWithSpacePadding() {
        final String target = "test";

        simpleSelect("   " + target + "   ", target);
    }

    @Test
    public void simpleSelectUsingJmxTarget() {
        final String target = "Coherence:type=Node,nodeId=*";

        simpleSelect(target, target);
    }

    @Test
    public void simpleSelectUsingJmxTargetWithSpacePadding() {
        final String target = "Coherence:type=Node,nodeId=*";

        simpleSelect("   " + target + "   ", target);
    }

    @Test
    public void jmxWithNoSelect() {
        final String target = "Coherence:type=Cache,service=DistributedCache,name=test,nodeId=1,tier=back";

        final QueryParser parser = new DefaultQueryParser(target);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        assertThat(parser.getProjection(), Is.<ValueExtractor>is(IdentityExtractor.INSTANCE));
    }

    @Test
    public void simpleWithNoSelect() {
        final String target = "test";
        final String query = format(target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        assertThat(parser.getProjection(), Is.<ValueExtractor>is(IdentityExtractor.INSTANCE));
    }

    @Test
    public void simpleSelectUsingJmxTargetUsingKeyInMapForProjection() {
        final String target = "Coherence:type=Node,nodeId=*";
        final String query = format("select get('nodeId') from %s", target);

        simpleSelectUsingKeyInMapForProjection(target, query);
    }

    @Test
    public void simpleSelectUsingUsingKeyInMapForProjection() {
        final String target = "node";
        final String query = format("select get('nodeId') from %s", target);

        simpleSelectUsingKeyInMapForProjection(target, query);
    }

    private void simpleSelectUsingKeyInMapForProjection(String target, String query) {
        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));
    }

    private void simpleSelect(final String target,
                              final String expectedTarget) {

        final String query = format("select a, b from %s", target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(expectedTarget));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        final ValueExtractor extractor = new MultiExtractor(new ValueExtractor[]{
                new ReflectionExtractor("getA"),
                new ReflectionExtractor("getB")
        });

        assertThat(parser.getProjection(), is(extractor));
    }

    @Test
    public void simpleSelectWhere() {
        final String target = "test";
        final String valueWithoutQuotes = "SomeValue";
        final String query = format("select a, b from %s where c = '%s'", target, valueWithoutQuotes);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));

        final Filter filter = new EqualsFilter("getC", valueWithoutQuotes);

        assertThat(parser.getRestriction(), is(filter));

        final ValueExtractor extractor = new MultiExtractor(new ValueExtractor[]{
                new ReflectionExtractor("getA"),
                new ReflectionExtractor("getB")
        });

        assertThat(parser.getProjection(), is(extractor));
    }

    @Test
    public void simpleSelectUsingKeyInMapForProjection() {
        final String keyInMap = "SomeKey";
        final String target = "test";
        final String query = format("select get('%s'), from %s ", keyInMap, target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));
        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        final ValueExtractor extractor = new ReflectionExtractor("get", new Object[]{keyInMap});
        assertThat(parser.getProjection(), is(extractor));
    }

    @Test
    public void simpleAggregation() {
        final String target = "test";
        final String query = format("select sum(c) from %s", target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(true));
        assertThat(parser.getProjection(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        final EntryAggregator aggregator = new DoubleSum("getC");

        assertThat(parser.getAggregation(), is(aggregator));
    }

    @Test
    public void simpleAggregationUsingKeyInMap() {
        final String target = "test";
        final String query = format("select sum(get('nodeId')) from %s", target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(true));
        assertThat(parser.getProjection(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        final EntryAggregator aggregator = new DoubleSum(new ReflectionExtractor("get", new String[]{"nodeId"}));

        assertThat(parser.getAggregation(), is(aggregator));
    }

    @Test
    @Ignore
    public void simpleDistinct() {
        final String target = "test";
        final String query = format("select distinct a, c from %s", target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(true));
        assertThat(parser.getProjection(), nullValue());

        assertThat(parser.getTarget(), is(target));

        assertThat(parser.getRestriction(), Is.<Filter>is(AlwaysFilter.INSTANCE));

        final EntryAggregator aggregator = new DoubleSum("getC");

        assertThat(parser.getAggregation(), is(aggregator));
    }
}
