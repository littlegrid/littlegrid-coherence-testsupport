package org.littlegrid.management.impl;

import com.tangosol.coherence.dslquery.CoherenceQueryLanguage;
import com.tangosol.coherence.dslquery.FilterBuilder;
import com.tangosol.coherence.dslquery.SQLOPParser;
import com.tangosol.coherence.dslquery.SelectListMaker;
import com.tangosol.coherence.dsltools.precedence.TokenTable;
import com.tangosol.coherence.dsltools.termtrees.AtomicTerm;
import com.tangosol.coherence.dsltools.termtrees.NodeTerm;
import com.tangosol.coherence.dsltools.termtrees.Term;
import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import com.tangosol.util.filter.AlwaysFilter;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;

/**
 * Management query parser default implementation with default scope.
 */
class DefaultQueryParser implements QueryParser {
    private static final TokenTable TOKEN_TABLE = CoherenceQueryLanguage.getSqlTokenTable(false);
    private static final String FROM_KEYWORD = "from";
    private static final String FIELD_LIST_KEYWORD = "fieldList";
    private static final String IS_DISTINCT_KEYWORD = "isDistinct";
    private static final String WHERE_CLAUSE_KEYWORD = "whereClause";

    private final ValueExtractor projection;
    private final EntryAggregator aggregation;
    private final String target;
    private final Filter restriction;

    /**
     * Constructor.
     *
     * @param query Query.
     */
    public DefaultQueryParser(final String query) {
        final SQLOPParser parser = new SQLOPParser(addFromTargetQuotes(query), TOKEN_TABLE);
        final NodeTerm term = (NodeTerm) parser.parse();

/*
        if (parseDistinctTerm(term).getFunctor().equals(IS_DISTINCT_KEYWORD)) {
            throw new UnsupportedOperationException();
        }
*/

        this.target = parseForTarget(term);
        this.restriction = parseForRestriction(term);
        this.projection = parseForProjection(term);
        this.aggregation = parseForAggregation(term);
    }

    private String addFromTargetQuotes(final String query) {
        final int fromKeywordStart = query.indexOf(FROM_KEYWORD);
        final int fromKeywordEndPlusSpace = fromKeywordStart + FROM_KEYWORD.length() + 1;
        final int nextSpaceAfterTarget = query.indexOf(" ", fromKeywordEndPlusSpace);

        final String projectionIncludingFromPlusSpace = query.substring(0, fromKeywordEndPlusSpace);
        final String queryWithQuotedTarget;

        if (nextSpaceAfterTarget == -1) {
            final String target = query.substring(fromKeywordEndPlusSpace);

            queryWithQuotedTarget = format("%s '%s'", projectionIncludingFromPlusSpace, target);
        } else  {
            final String target = query.substring(fromKeywordEndPlusSpace, nextSpaceAfterTarget);
            final String afterTarget = query.substring(nextSpaceAfterTarget);

            queryWithQuotedTarget = format("%s '%s' %s", projectionIncludingFromPlusSpace, target, afterTarget);
        }

        System.out.println("Quoted query: " + queryWithQuotedTarget);

        return queryWithQuotedTarget;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ValueExtractor getProjection() {
        return projection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTarget() {
        return target;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Filter getRestriction() {
        return restriction;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isAggregation() {
        return (aggregation != null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntryAggregator getAggregation() {
        return aggregation;
    }

    private String parseForTarget(final NodeTerm term) {
        return atomicStringValueOf(term.findAttribute(FROM_KEYWORD));
    }

    private Filter parseForRestriction(final NodeTerm term) {
        final Term restrictionTerm = parseRestrictionTerm(term);

        if (restrictionTerm == null) {
            return AlwaysFilter.INSTANCE;
        } else {
            return new FilterBuilder(restrictionTerm).makeFilter();
        }
    }

    private ValueExtractor parseForProjection(final NodeTerm term) {
        final Term projectionTerm = parseProjectionTerm(term);
        final SelectListMaker selectMaker = new SelectListMaker((NodeTerm) projectionTerm);
        selectMaker.makeSelects();

        return selectMaker.getResultsAsValueExtractor();
    }

    private EntryAggregator parseForAggregation(final NodeTerm term) {
        final Term aggregationTerm = parseAggregationTerm(term);
        final SelectListMaker selectMaker = new SelectListMaker((NodeTerm) aggregationTerm);
        selectMaker.makeSelects();

        return selectMaker.getResultsAsEntryAggregator();
    }

    private Term parseAggregationTerm(NodeTerm term) {
        return term.findChild(FIELD_LIST_KEYWORD);
    }

    private Term parseDistinctTerm(NodeTerm term) {
        return term.findChild(IS_DISTINCT_KEYWORD);
    }

    private Term parseProjectionTerm(final NodeTerm term) {
        return term.findChild(FIELD_LIST_KEYWORD);
    }

    private String atomicStringValueOf(final Term term) {
        if (term == null) {
            return null;
        }

        if (term.isAtom()) {
            return ((AtomicTerm) term).getValue();
        } else {
            return null;
        }
    }

    private Term parseRestrictionTerm(final NodeTerm term) {
        final Term child = term.findChild(WHERE_CLAUSE_KEYWORD);

        if (child == null) {
            return null;
        }

        if (child.length() == 1) {
            return child.termAt(1);
        } else {
            return null;
        }
    }
}
