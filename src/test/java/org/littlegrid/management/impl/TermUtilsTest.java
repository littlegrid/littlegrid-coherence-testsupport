package org.littlegrid.management.impl;

import com.tangosol.coherence.dslquery.AbstractCoherenceQueryWalker;
import com.tangosol.coherence.dslquery.SelectListMaker;
import com.tangosol.coherence.dsltools.termtrees.AtomicTerm;
import com.tangosol.coherence.dsltools.termtrees.Term;
import com.tangosol.coherence.dsltools.termtrees.Terms;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Collection;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.management.impl.TermUtils.CALL_NODE_TERM_KEYWORD;
import static org.littlegrid.management.impl.TermUtils.FIELD_LIST_TERM_KEYWORD;
import static org.littlegrid.management.impl.TermUtils.GET_TERM_KEYWORD;
import static org.littlegrid.management.impl.TermUtils.LITERAL_TERM_KEYWORD;
import static org.littlegrid.management.impl.TermUtils.WHERE_CLAUSE_TERM_KEYWORD;

@Ignore
public class TermUtilsTest {
    @Test(expected = IllegalArgumentException.class)
    public void getWhenNotFieldList() {
        TermUtils.getAllLiterals(Terms.newTerm("not-a-field-list"));
    }

    @Test
    public void noLiteralToGet() {
        final Collection<String> terms = TermUtils.getAllLiterals(Terms.newTerm(FIELD_LIST_TERM_KEYWORD));

        assertThat(terms.size(), is(0));
    }

    @Test
    public void oneLiteralToGet() {
        final String columnName = "columnA";

        final Collection<String> terms = TermUtils.getAllLiterals(getPopulatedFieldListWithOneLiteral(columnName));

        assertThat(terms.size(), is(1));

        assertThat(terms.iterator().next(), is(columnName));
    }

    @Test
    public void noLiteralsToConvert() {
        final Term term = Terms.newTerm(FIELD_LIST_TERM_KEYWORD);

        final MyTermWalker walker = new MyTermWalker(getPopulatedFieldListWithOneLiteral("a"));
//        walker.makeSelects((com.tangosol.coherence.dsltools.termtrees.NodeTerm) getPopulatedFieldListWithOneLiteral("a"));
        walker.makeSelects();

        final Term converted = TermUtils.convertFieldListLiteralsToGettersOnMap(term);

        throw new UnsupportedOperationException();
    }

    @Test
    public void oneLiteralToConvert() {
        final String columnName = "column1";

        final Term term = getPopulatedFieldListWithOneLiteral(columnName);

        final Term fieldList = TermUtils.convertFieldListLiteralsToGettersOnMap(term);
        assertThat(fieldList.getFunctor(), is(FIELD_LIST_TERM_KEYWORD));

        final Term[] fieldListChildren = fieldList.children();
        assertThat(fieldListChildren.length, is(1));

        checkCallNodeAsExpected(fieldListChildren[0], columnName);
    }

    private static Term getPopulatedFieldListWithOneLiteral(final String columnName) {
        return Terms.newTerm(FIELD_LIST_TERM_KEYWORD)
                .withChild(Terms.newTerm(LITERAL_TERM_KEYWORD)
                        .withChild(new AtomicTerm(columnName, AtomicTerm.SYMBOLLITERAL)));
    }

    private void checkCallNodeAsExpected(final Term callNode,
                                         final String columnName) {

        assertThat(callNode.getFunctor(), is(CALL_NODE_TERM_KEYWORD));

        final Term[] callNodeChildren = callNode.children();
        assertThat(callNodeChildren.length, is(1));

        final Term get = callNodeChildren[0];
        assertThat(get.getFunctor(), is(GET_TERM_KEYWORD));

        final Term[] getChildren = get.children();
        assertThat(getChildren.length, is(1));

        final Term literal = getChildren[0];
        assertThat(literal.getFunctor(), is(LITERAL_TERM_KEYWORD));

        final Term[] literalChildren = literal.children();
        assertThat(literalChildren.length, is(1));

        final Term value = literalChildren[0];
        assertThat(value.getClass().getName(), is(AtomicTerm.class.getName()));

        final AtomicTerm atomic = (AtomicTerm) value;
        assertThat(atomic.getValue(), is(columnName));
        assertThat(atomic.getTypeCode(), is(AtomicTerm.STRINGLITERAL));
    }

    @Test
    public void severalLiteralsToConvert() {

    }

    public static class MyTermWalker extends SelectListMaker {
        public MyTermWalker(Term a) {
            super((com.tangosol.coherence.dsltools.termtrees.NodeTerm) a);
        }

        @Override
        public void acceptAtom(String sFunctor, AtomicTerm atom) {
            System.out.println("HERE");
            super.acceptAtom(sFunctor, atom);
        }
    }
}
