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

import org.junit.Before;
import org.junit.Test;

import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

/**
 * System utilities tests.
 */
public final class SystemUtilsTest {
    private static final String KNOWN_PREFIX = "prefix.used.for.testing.";
    private static final String KEY1_WITH_KNOWN_PREFIX = KNOWN_PREFIX + "key1";
    private static final String KNOWN_VALUE_1 = "value1";
    private static final String KEY2_WITH_KNOWN_PREFIX = KNOWN_PREFIX + "key2";
    private static final String KNOWN_VALUE_2 = "value2";

    @Before
    public void clearSystemProperties() {
        System.clearProperty(KEY1_WITH_KNOWN_PREFIX);
        System.clearProperty(KEY2_WITH_KNOWN_PREFIX);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void construct() {
        new SystemUtils();
    }

    @Test
    public void getPrefixedSystemProperties() {
        clearSystemProperties();

        {
            Properties systemPropertiesWithPrefix = SystemUtils.getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(systemPropertiesWithPrefix.size(), is(0));
        }

        String key = KEY2_WITH_KNOWN_PREFIX;
        String value = KNOWN_VALUE_2;

        System.setProperty(key, value);

        {
            Properties systemPropertiesWithPrefix = SystemUtils.getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(systemPropertiesWithPrefix.size(), is(1));

            Map.Entry<String, String> entry = getFirstEntry(systemPropertiesWithPrefix);

            assertThat(entry.getKey(), is(key));
            assertThat(entry.getValue(), is(value));
        }
    }

    @Test
    public void snapshotSystemProperties() {
        Properties propertiesBeforeChange = SystemUtils.snapshotSystemProperties();

        final int beforeChangeSize = propertiesBeforeChange.size();

        System.setProperty(KEY1_WITH_KNOWN_PREFIX, KNOWN_VALUE_1);

        assertThat(System.getProperties().size(), not(beforeChangeSize));
        assertThat(System.getProperties(), not(propertiesBeforeChange));
    }

    @Test
    public void applyToSystemProperties() {
        final String key = KEY1_WITH_KNOWN_PREFIX;
        final String value = KNOWN_VALUE_1;

        Properties properties = new Properties();
        properties.setProperty(key, value);

        SystemUtils.applyToSystemProperties(properties);

        assertThat(System.getProperties().containsKey(key), is(true));
        assertThat(System.getProperty(key), is(value));
    }

    @Test(expected = IllegalArgumentException.class)
    public void applyToSystemPropertiesWhenKeyIsEmpty() {
        final String key = "";
        final String value = KNOWN_VALUE_1;

        Properties properties = new Properties();
        properties.setProperty(key, value);

        SystemUtils.applyToSystemProperties(properties);
    }

    @Test
    public void applyToSystemPropertiesWhenValueIsEmpty() {
        final String key = KEY1_WITH_KNOWN_PREFIX;
        final String value = "";

        Properties properties = new Properties();
        properties.setProperty(key, value);

        SystemUtils.applyToSystemProperties(properties);

        assertThat(System.getProperties().containsKey(key), is(false));
    }

    @Test
    public void applyToSystemPropertiesWhenValueIsJustSpaces() {
        final String key = KEY1_WITH_KNOWN_PREFIX;
        final String value = "  ";

        Properties properties = new Properties();
        properties.setProperty(key, value);

        SystemUtils.applyToSystemProperties(properties);

        assertThat(System.getProperties().containsKey(key), is(false));
    }

    @Test
    public void getPrefixedProperties() {
        final String key = KEY1_WITH_KNOWN_PREFIX;
        final String value = KNOWN_VALUE_1;

        final Properties properties = new Properties();

        for (int i = 0; i < 5; i++) {
            properties.setProperty("key-" + i, "non-prefixed property");
        }

        properties.setProperty(key, value);

        final Properties prefixedProperties = SystemUtils.getPropertiesWithPrefix(properties, KNOWN_PREFIX, false);

        assertThat(prefixedProperties.size(), is(1));
        assertThat(prefixedProperties.getProperty(key), is(value));
    }

    @Test
    public void getPrefixedPropertiesWithPrefixRemoved() {
        final String keyWithPrefix = KEY1_WITH_KNOWN_PREFIX;
        final String keyWithoutPrefix = keyWithPrefix.replaceAll(KNOWN_PREFIX, "");
        final String value = KNOWN_VALUE_1;

        final Properties properties = new Properties();

        for (int i = 0; i < 5; i++) {
            properties.setProperty("key-" + i, "non-prefixed property");
        }

        properties.setProperty(keyWithPrefix, value);

        final Properties prefixedProperties = SystemUtils.getPropertiesWithPrefix(properties, KNOWN_PREFIX, true);

        assertThat(prefixedProperties.size(), is(1));
        assertThat(prefixedProperties.getProperty(keyWithoutPrefix), is(value));
    }

    @Test
    public void getPrefixedPropertiesWhenPropertiesIsEmpty() {
        final Properties prefixedProperties =
                SystemUtils.getPropertiesWithPrefix(new Properties(), KNOWN_PREFIX, false);

        assertThat(prefixedProperties.size(), is(0));
    }

    @Test
    public void getEnvironmentVariables() {
        final Properties environmentVariables = SystemUtils.getEnvironmentVariables();

        assertThat(environmentVariables.size() > 0, is(true));
    }

    @SuppressWarnings("unchecked")
    private static Map.Entry<String, String> getFirstEntry(final Properties properties) {
        Set<Map.Entry<Object, Object>> entries = properties.entrySet();

        return (Map.Entry<String, String>) entries.toArray()[0];
    }
}
