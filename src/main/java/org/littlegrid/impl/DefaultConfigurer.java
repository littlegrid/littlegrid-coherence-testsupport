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

package org.littlegrid.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Default configurer.
 *
 * @since 2.16
 */
class DefaultConfigurer extends ImmutableConfigurer {
    /**
     * Constructor.
     */
    public DefaultConfigurer() {
        super(new HashMap<String, String>(), new Properties(), new Properties());
    }

    void setBuilderValue(final String key,
                         final boolean value) {

        getDirectMutableAccessToBuilderKeysAndValues().put(key, Boolean.toString(value));
    }

    void setBuilderValue(final String key,
                         final int value) {

        getDirectMutableAccessToBuilderKeysAndValues().put(key, Integer.toString(value));
    }

    void setBuilderValue(final String key,
                         final long value) {

        getDirectMutableAccessToBuilderKeysAndValues().put(key, Long.toString(value));
    }

    void setBuilderValue(final String key,
                         final String value) {

        getDirectMutableAccessToBuilderKeysAndValues().put(key, value);
    }

    public void setAdditionalSystemProperties(final Properties additionalSystemProperties) {
        // Pass them through individually so any bad key/values are identified
        for (final Map.Entry entry : additionalSystemProperties.entrySet()) {
            setAdditionalSystemProperty((String) entry.getKey(), (String) entry.getValue());
        }
    }

    public void setAdditionalSystemProperty(final String key,
                                            final String value) {

        if (key == null) {
            throw new IllegalArgumentException(String.format("Key cannot be null, supplied value was: '%s'", value));
        }

        if (value == null) {
            throw new IllegalArgumentException(String.format("Value cannot be null, supplied key was: '%s'", key));
        }

        getDirectMutableAccessToAdditionalSystemProperties().put(key, value);
    }

    public void setBuilderKeyToSystemPropertyNameMapping(final Properties builderKeyToSystemPropertyNameMapping) {
        // Pass them through individually so any bad key/values are identified
        for (final Map.Entry entry : builderKeyToSystemPropertyNameMapping.entrySet()) {
            setBuilderKeyToSystemPropertyNameMapping((String) entry.getKey(), (String) entry.getValue());
        }
    }

    public void setBuilderKeyToSystemPropertyNameMapping(final String key,
                                                         final String value) {

        if (key == null) {
            throw new IllegalArgumentException(String.format("Key cannot be null, supplied value was: '%s'", value));
        }

        if (value == null) {
            throw new IllegalArgumentException(String.format("Value cannot be null, supplied key was: '%s'", key));
        }

        getDirectMutableAccessToBuilderKeyToSystemPropertyNameMapping().put(key, value);
    }
}
