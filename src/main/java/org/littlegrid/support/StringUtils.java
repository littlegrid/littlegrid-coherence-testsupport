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

/**
 * String utilities class.
 *
 * @since 2.16
 */
public final class StringUtils {
    /**
     * Default scope to enable test coverage.
     */
    StringUtils() {
        throw new UnsupportedOperationException();
    }

    /**
     * Checks if the string has a value (as in it is not null and it contains characters
     * beyond just spaces).
     *
     * @param stringToCheckForValue String to check.
     * @return true if characters exist.
     */
    public static boolean stringHasValue(final String stringToCheckForValue) {
        return stringToCheckForValue != null && stringToCheckForValue.trim().length() > 0;
    }

    /**
     * Converts strings in an array to a comma-delimited string.
     *
     * @param strings String to concatenate.
     * @return comma-delimited string.
     */
    public static String stringArrayToCommaDelimitedString(final String[] strings) {
        final StringBuilder sb = new StringBuilder();

        int count = 0;

        for (final String string : strings) {
            if (count > 0) {
                sb.append(",");
            }

            sb.append(string);
            count++;
        }

        return sb.toString();
    }
}
