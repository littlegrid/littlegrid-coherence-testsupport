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
        final SQLOPParser parser = new SQLOPParser(query, TOKEN_TABLE);
        final NodeTerm term = (NodeTerm) parser.parse();

/*
        if (parseDistinctTerm(term).getFunctor().equals(IS_DISTINCT_KEYWORD)) {
            throw new UnsupportedOperationException();
        }
*/

        this.target = parseAndSetTarget(term);
        this.restriction = parseAndSetRestriction(term);
        this.projection = parseAndSetProjection(term);
        this.aggregation = parseAndSetAggregation(term);
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

    private String parseAndSetTarget(final NodeTerm term) {
        return atomicStringValueOf(term.findAttribute(FROM_KEYWORD));
    }

    private Filter parseAndSetRestriction(final NodeTerm term) {
        final Term restrictionTerm = parseRestrictionTerm(term);

        if (restrictionTerm == null) {
            return AlwaysFilter.INSTANCE;
        } else {
            return new FilterBuilder(restrictionTerm).makeFilter();
        }
    }

    private ValueExtractor parseAndSetProjection(final NodeTerm term) {
        final Term projectionTerm = parseProjectionTerm(term);
        final SelectListMaker selectMaker = new SelectListMaker((NodeTerm) projectionTerm);
        selectMaker.makeSelects();

        return selectMaker.getResultsAsValueExtractor();
    }

    private EntryAggregator parseAndSetAggregation(final NodeTerm term) {
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
