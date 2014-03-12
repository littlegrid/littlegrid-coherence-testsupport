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

package org.littlegrid.support;

import java.util.Map;
import java.util.Properties;

import static java.lang.String.format;

/**
 * System utilities class providing useful system related methods.
 */
public final class SystemUtils {
    /**
     * Default scope to enable test coverage.
     */
    SystemUtils() {
        throw new UnsupportedOperationException();
    }

    /**
     * Captures current system properties.
     *
     * @return current properties.
     */
    public static Properties snapshotSystemProperties() {
        final Properties properties = new Properties();

        for (final Object object : System.getProperties().keySet()) {
            final String key = (String) object;
            final String value = System.getProperty(key);

            properties.setProperty(key, value);
        }

        return properties;
    }

    /**
     * Apply the properties to the system properties.
     *
     * @param properties New and updated system properties.
     */
    public static void applyToSystemProperties(final Properties properties) {
        for (final Object object : properties.keySet()) {
            final String key = (String) object;
            final String value = properties.getProperty(key);

            // Key cannot be null because properties extends hash table
            if (key.trim().length() == 0) {
                throw new IllegalArgumentException(format("Key cannot be empty string for value of: '%s'", value));
            }

            if (value.trim().length() > 0) {
                System.setProperty(key, value);
            }
        }
    }

    /**
     * Get current system properties which start with the specified prefix.
     *
     * @param prefix Prefix.
     * @return properties.
     */
    public static Properties getSystemPropertiesWithPrefix(final String prefix) {
        return getPropertiesWithPrefix(System.getProperties(), prefix, false);
    }

    /**
     * Get the properties which start with the specified prefix.
     *
     * @param propertiesToCheck Properties.
     * @param prefix            Prefix.
     * @param removePrefix      If true, then properties returned have the prefix removed.
     * @return properties Properties.
     * @since 2.7
     */
    public static Properties getPropertiesWithPrefix(final Properties propertiesToCheck,
                                                     final String prefix,
                                                     final boolean removePrefix) {

        final Properties properties = new Properties();

        for (final Object object : propertiesToCheck.keySet()) {
            final String key = (String) object;

            if (key.contains(prefix)) {
                String keyToUse = key;

                if (removePrefix) {
                    keyToUse = key.replaceAll(prefix, "");
                }

                final String value = propertiesToCheck.getProperty(key);

                properties.setProperty(keyToUse, value);
            }
        }

        return properties;
    }

    /**
     * Get the environment variables as properties.
     *
     * @return properties.
     * @since 2.7
     */
    public static Properties getEnvironmentVariables() {
        final Properties environmentVariables = new Properties();

        for (final Map.Entry<String, String> entry : System.getenv().entrySet()) {
            environmentVariables.setProperty(entry.getKey(), entry.getValue());
        }

        return environmentVariables;
    }
}
