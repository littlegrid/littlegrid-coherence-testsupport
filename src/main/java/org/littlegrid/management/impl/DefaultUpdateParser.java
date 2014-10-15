package org.littlegrid.management.impl;

import com.tangosol.coherence.dslquery.CoherenceQueryLanguage;
import com.tangosol.coherence.dslquery.SQLOPParser;
import com.tangosol.coherence.dsltools.precedence.TokenTable;
import com.tangosol.coherence.dsltools.termtrees.NodeTerm;
import com.tangosol.util.Filter;

/**
 * Default update parser.
 */
public class DefaultUpdateParser implements UpdateParser {
    private static final TokenTable TOKEN_TABLE = CoherenceQueryLanguage.getSqlTokenTable(false);

    public DefaultUpdateParser(String update) {
        final SQLOPParser parser = new SQLOPParser(update, TOKEN_TABLE);
        final NodeTerm term = (NodeTerm) parser.parse();
    }

    @Override
    public String getTarget() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Filter getRestriction() {
        throw new UnsupportedOperationException();
    }
}
