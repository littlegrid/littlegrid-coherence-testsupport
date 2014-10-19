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

import javax.xml.soap.Node;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;

/**
 * Management query parser default implementation.
 */
class DefaultQueryParser implements QueryParser {
    private static final TokenTable TOKEN_TABLE = CoherenceQueryLanguage.getSqlTokenTable(false);
    private static final String SPACE = " ";
    private static final String SELECT_KEYWORD = "select";
    private static final String SELECT_KEYWORD_PLUS_SPACE = SELECT_KEYWORD + SPACE;
    private static final String FROM_KEYWORD = "from";
    private static final String SPACE_FROM_KEYWORD_PLUS_SPACE = SPACE + FROM_KEYWORD + SPACE;
    private static final String SPACE_WHERE_KEYWORD_PLUS_SPACE = SPACE + "where" + SPACE;
    private static final String SPACE_GROUP_BY_WHERE = SPACE + "group by" + SPACE;
    private static final String COHQL_FIELD_LIST_KEYWORD = "fieldList";
    private static final String COHQL_IS_DISTINCT_KEYWORD = "isDistinct";
    private static final String COHQL_WHERE_CLAUSE_KEYWORD = "whereClause";

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
        final String queryEnsuredFrom = ensureFromIsPresent(query);
        final String queryEnsuredSelect = ensureSelectIsPresent(queryEnsuredFrom);
        final String queryEnsuredQuotes = ensureFromTargetHasQuotes(queryEnsuredSelect);
        final SQLOPParser parser = new SQLOPParser(queryEnsuredQuotes, TOKEN_TABLE);
        final NodeTerm nodeTerm;

        try {
            nodeTerm = (NodeTerm) parser.parse();

        } catch (RuntimeException e) {
            throw new IllegalArgumentException(format("Cannot parse query: %s due to: %s", queryEnsuredQuotes, e));
        }

/*
        if (parseDistinctTerm(term).getFunctor().equals(COHQL_IS_DISTINCT_KEYWORD)) {
            throw new UnsupportedOperationException();
        }
*/

        this.target = parseForTarget(nodeTerm);
        this.restriction = parseForRestriction(nodeTerm);
        this.projection = parseForProjection(nodeTerm);
        this.aggregation = parseForAggregation(nodeTerm);
    }

    static String ensureFromTargetHasQuotes(final String query) {
        final String trimmedQuery = query.trim();
        final String lowerCaseTrimmedQuery = trimmedQuery.toLowerCase();
        final int fromStartPosition = lowerCaseTrimmedQuery.indexOf(SPACE_FROM_KEYWORD_PLUS_SPACE);

        if (fromStartPosition == -1) {
            throw new IllegalArgumentException(format("Cannot find keyword: '%s' with query: '%s'",
                    SPACE_FROM_KEYWORD_PLUS_SPACE, query));
        }

        final int fromEndPositionPlusSpaceImpliedTargetStart =
                fromStartPosition + SPACE_FROM_KEYWORD_PLUS_SPACE.length();

        final int whereStartPosition =  lowerCaseTrimmedQuery.indexOf(SPACE_WHERE_KEYWORD_PLUS_SPACE);
        final int targetEndPosition;

        if (whereStartPosition == - 1) {
            final int groupByStartPosition = lowerCaseTrimmedQuery.indexOf(SPACE_GROUP_BY_WHERE,
                    fromEndPositionPlusSpaceImpliedTargetStart);

            if (groupByStartPosition == -1) {
                targetEndPosition = trimmedQuery.length();
            } else {
                targetEndPosition = groupByStartPosition;
            }
        } else {
            targetEndPosition = whereStartPosition;
        }

        final String trimmedCandidateTarget = trimmedQuery.substring(fromEndPositionPlusSpaceImpliedTargetStart,
                targetEndPosition).trim();

        final String quotedTarget = format("'%s'", trimmedCandidateTarget);
        final String beforeTarget = trimmedQuery.substring(0, fromEndPositionPlusSpaceImpliedTargetStart);
        final String afterTarget = trimmedQuery.substring(targetEndPosition);

        final String queryWithQuotedTarget = beforeTarget + quotedTarget + afterTarget;

        return queryWithQuotedTarget;
    }

    static String ensureFromIsPresent(final String query) {
        return ensureKeywordIsPresent(query, SPACE_FROM_KEYWORD_PLUS_SPACE, SPACE_FROM_KEYWORD_PLUS_SPACE);
    }

    static String ensureSelectIsPresent(final String query) {
        return ensureKeywordIsPresent(query, SELECT_KEYWORD_PLUS_SPACE, SELECT_KEYWORD_PLUS_SPACE + "value() ");
    }

    static String ensureKeywordIsPresent(final String query,
                                         final String expectedKeywordToBePresent,
                                         final String prefixToUseIfKeywordNotPresent) {

        final String lowerCaseQuery = query.toLowerCase();

        if (lowerCaseQuery.contains(expectedKeywordToBePresent)) {
            return query;
        } else {
            return prefixToUseIfKeywordNotPresent + query;
        }
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
        final Term restrictionTerm = parseForRestrictionTerm(term);

        if (restrictionTerm == null) {
            return AlwaysFilter.INSTANCE;
        } else {
            return new FilterBuilder(restrictionTerm).makeFilter();
        }
    }

    private ValueExtractor parseForProjection(final NodeTerm term) {
        final NodeTerm projectionTerm = parseForProjectionTerm(term);
        final SelectListMaker selectMaker = new SelectListMaker(projectionTerm);
        selectMaker.makeSelects();

        return selectMaker.getResultsAsValueExtractor();
    }

    private EntryAggregator parseForAggregation(final NodeTerm term) {
        final Term aggregationTerm = parseForAggregationTerm(term);
        final SelectListMaker selectMaker = new SelectListMaker((NodeTerm) aggregationTerm);
        selectMaker.makeSelects();

        return selectMaker.getResultsAsEntryAggregator();
    }

    private Term parseForAggregationTerm(NodeTerm term) {
        return term.findChild(COHQL_FIELD_LIST_KEYWORD);
    }

    private Term parseDistinctTerm(NodeTerm term) {
        return term.findChild(COHQL_IS_DISTINCT_KEYWORD);
    }

    private NodeTerm parseForProjectionTerm(final NodeTerm term) {
        return (NodeTerm) term.findChild(COHQL_FIELD_LIST_KEYWORD);
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

    private Term parseForRestrictionTerm(final NodeTerm term) {
        final Term child = term.findChild(COHQL_WHERE_CLAUSE_KEYWORD);

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
