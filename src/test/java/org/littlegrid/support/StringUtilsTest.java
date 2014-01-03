/*
 * Copyright (c) 2010-2013 Jonathan Hall.
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

package org.littlegrid.support;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * String utilities tests.
 */
public class StringUtilsTest {
    @Test(expected = UnsupportedOperationException.class)
    public void construct() {
        new StringUtils();
    }

    @Test
    public void stringHasValueWhenNull() {
        assertThat(StringUtils.stringHasValue(null), is(false));
    }

    @Test
    public void stringHasValueWhenContainsSpaces() {
        assertThat(StringUtils.stringHasValue("   "), is(false));
    }

    @Test
    public void stringHasValueWhenContainsCharacters() {
        assertThat(StringUtils.stringHasValue("ABC"), is(true));
    }

    @Test
    public void stringArrayToCommaDelimitedWhenNoStrings() {
        assertThat(StringUtils.stringArrayToCommaDelimitedString(new String[]{}), is(""));
    }

    @Test
    public void stringArrayToCommaDelimitedWhenJustOneString() {
        final String[] strings = {"abc"};
        final String expectedResult = strings[0];

        assertThat(StringUtils.stringArrayToCommaDelimitedString(strings), is(expectedResult));
    }

    @Test
    public void stringArrayToCommaDelimitedWhenManyStrings() {
        final String[] strings = {"abc", "def"};
        final String expectedResult = strings[0] + "," + strings[1];

        assertThat(StringUtils.stringArrayToCommaDelimitedString(strings), is(expectedResult));
    }
}
