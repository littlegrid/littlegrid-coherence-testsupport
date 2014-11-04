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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;

/**
 * Management query parser default implementation.
 *
 * @since 2.16
 */
class DefaultQueryParser implements QueryParser {
    private static final String ATTRIBUTE_NAME_DEFAULT_PATTERN = "(@\\w*)";
    private static final String ATTRIBUTE_NAME_DEFAULT_INDICATOR = "@";

    private static final String FIELD_LIST_TERM_KEYWORD = "fieldList";
    private static final String IS_DISTINCT_TERM_KEYWORD = "isDistinct";
    private static final String WHERE_CLAUSE_TERM_KEYWORD = "whereClause";

    private static final TokenTable TOKEN_TABLE = CoherenceQueryLanguage.getSqlTokenTable(false);
    private static final String SINGLE_SPACE = " ";
    private static final String SINGLE_QUOTE = "'";
    private static final String SELECT_KEYWORD = "select";
    private static final String FROM_KEYWORD = "from";
    private static final String WHERE_KEYWORD = "where";
    private static final String GROUP_BY_KEYWORD = "group by";
    private static final String SELECT_PLUS_SPACE = SELECT_KEYWORD + SINGLE_SPACE;
    private static final String SPACE_FROM_PLUS_SPACE = SINGLE_SPACE + FROM_KEYWORD + SINGLE_SPACE;
    private static final String SPACE_WHERE_PLUS_SPACE = SINGLE_SPACE + WHERE_KEYWORD + SINGLE_SPACE;
    private static final String SPACE_GROUP_BY_SPACE = SINGLE_SPACE + GROUP_BY_KEYWORD + SINGLE_SPACE;

    private final ValueExtractor projection;
    private final EntryAggregator aggregation;
    private final String target;
    private final Filter restriction;

    /**
     * Constructor.
     *
     * @param attributeNamePattern   Attribute name pattern.
     * @param attributeNameIndicator Attribute name indicator.
     * @param query                  Query.
     */
    public DefaultQueryParser(final String attributeNamePattern,
                              final String attributeNameIndicator,
                              final String query) {

        final String queryEnsuredFrom = ensureFromIsPresent(query);
        final String queryEnsuredSelect = ensureSelectIsPresent(queryEnsuredFrom);
        final String queryEnsuredQuotes = ensureFromTargetHasQuotes(queryEnsuredSelect);
        final String queryEnsuredGetters = ensureAttributesConvertedToMapGets(
                attributeNamePattern, attributeNameIndicator, queryEnsuredQuotes);

        final SQLOPParser parser = new SQLOPParser(queryEnsuredGetters, TOKEN_TABLE);
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

    /**
     * Constructor.
     *
     * @param query Query.
     */
    public DefaultQueryParser(final String query) {
        this(ATTRIBUTE_NAME_DEFAULT_PATTERN, ATTRIBUTE_NAME_DEFAULT_INDICATOR, query);
    }

    static String ensureAttributesConvertedToMapGets(final String attributeNamePattern,
                                                     final String attributeNameIndicator,
                                                     final String query) {

        final Pattern pattern = Pattern.compile(attributeNamePattern);
        final Matcher matcher = pattern.matcher(query);

        String result = query;

        while (matcher.find()) {
            final String group = matcher.group();
            final String translatedAttribute = group.replaceAll(attributeNameIndicator, "");

            result = result.replaceFirst(group, "get('" + translatedAttribute + "')");
        }

        return result;
    }

    static String ensureFromTargetHasQuotes(final String query) {
        final String trimmedQuery = query.trim();
        final String lowerCaseTrimmedQuery = trimmedQuery.toLowerCase();
        final int fromStartPosition = lowerCaseTrimmedQuery.indexOf(SPACE_FROM_PLUS_SPACE);

        if (fromStartPosition == -1) {
            throw new IllegalArgumentException(format("Cannot find keyword: '%s' with query: '%s'",
                    SPACE_FROM_PLUS_SPACE, query));
        }

        final int fromEndPositionPlusSpaceImpliedTargetStart =
                fromStartPosition + SPACE_FROM_PLUS_SPACE.length();

        final int whereStartPosition = lowerCaseTrimmedQuery.indexOf(SPACE_WHERE_PLUS_SPACE);
        final int targetEndPosition;

        if (whereStartPosition == -1) {
            final int groupByStartPosition = lowerCaseTrimmedQuery.indexOf(SPACE_GROUP_BY_SPACE,
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

        final String quotedTarget = SINGLE_QUOTE + trimmedCandidateTarget + SINGLE_QUOTE;
        final String beforeTarget = trimmedQuery.substring(0, fromEndPositionPlusSpaceImpliedTargetStart);
        final String afterTarget = trimmedQuery.substring(targetEndPosition);

        final String queryWithQuotedTarget = beforeTarget + quotedTarget + afterTarget;

        return queryWithQuotedTarget;
    }

    static String ensureFromIsPresent(final String query) {
        return ensureKeywordIsPresent(query, SPACE_FROM_PLUS_SPACE, SPACE_FROM_PLUS_SPACE);
    }

    static String ensureSelectIsPresent(final String query) {
        return ensureKeywordIsPresent(query, SELECT_PLUS_SPACE, SELECT_PLUS_SPACE + "value() ");
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
        return term.findChild(FIELD_LIST_TERM_KEYWORD);
    }

    private Term parseDistinctTerm(NodeTerm term) {
        return term.findChild(IS_DISTINCT_TERM_KEYWORD);
    }

    private NodeTerm parseForProjectionTerm(final NodeTerm term) {
        return (NodeTerm) term.findChild(FIELD_LIST_TERM_KEYWORD);
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
        final Term child = term.findChild(WHERE_CLAUSE_TERM_KEYWORD);
        final int childCount = child.length();

        if (childCount == 0) {
            return null;
        } else if (childCount == 1) {
            return child.termAt(childCount);
        } else {
            return null;
        }
    }
}
