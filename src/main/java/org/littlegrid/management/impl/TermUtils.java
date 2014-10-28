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

import com.tangosol.coherence.dsltools.termtrees.Term;

import java.util.ArrayList;
import java.util.Collection;

import static java.lang.String.format;

/**
 * Terms utilities.
 */
public class TermUtils {
    static final String FIELD_LIST_TERM_KEYWORD = "fieldList";
    static final String IS_DISTINCT_TERM_KEYWORD = "isDistinct";
    static final String WHERE_CLAUSE_TERM_KEYWORD = "whereClause";
    static final String LITERAL_TERM_KEYWORD = "literal";
    static final String CALL_NODE_TERM_KEYWORD = "callNode";
    static final String GET_TERM_KEYWORD = "get";

    /**
     * Default scope to enable test coverage.
     */
    TermUtils() {
        throw new UnsupportedOperationException();
    }

    static Term convertFieldListLiteralsToGettersOnMap(final Term term) {
        if (!term.getFunctor().equals(FIELD_LIST_TERM_KEYWORD)) {
            throw new UnsupportedOperationException();
        }

//        final


        throw new UnsupportedOperationException();
    }

    static Collection<String> getAllLiterals(final Term term) {
        final String functor = term.getFunctor();

        if (!functor.equals(FIELD_LIST_TERM_KEYWORD)) {
            throw new IllegalArgumentException(format("Argument was not a: %s, but instead was wrongly: %s",
                    FIELD_LIST_TERM_KEYWORD, functor));
        }

        final Collection<String> literals = new ArrayList<String>();


        return literals;
    }
}
