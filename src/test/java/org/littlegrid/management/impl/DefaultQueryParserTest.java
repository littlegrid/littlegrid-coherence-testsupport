package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import com.tangosol.util.aggregator.DoubleSum;
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

/**
 * Management query parser implementation tests.
 */
public class DefaultQueryParserTest {
    //TODO: include various tests where there are lots of spaces

    @Test
    public void simpleSelect() {
        final String target = "test";

        simpleSelect(target);
    }

    @Test
    public void simpleSelectUsingJmxTarget() {
        final String target = "Coherence:type=Node,nodeId=*";

        simpleSelect(target);
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

    private void simpleSelect(final String target) {
        final String query = format("select a, b from %s", target);

        final QueryParser parser = new DefaultQueryParser(query);

        assertThat(parser.isAggregation(), is(false));
        assertThat(parser.getAggregation(), nullValue());

        assertThat(parser.getTarget(), is(target));

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
