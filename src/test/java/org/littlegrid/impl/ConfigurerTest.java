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

package org.littlegrid.impl;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY;

/**
 * Configurer tests.
 */
public class ConfigurerTest {
    @Test
    public void constructWithNoEntries() {
        final DefaultConfigurer configurer = getEmptyConfigurer();

        assertThat(configurer.getBuilderKeysAndValues().size(), is(0));
        assertThat(configurer.getAdditionalSystemProperties().size(), is(0));
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings().size(), is(0));
    }

    @Test
    public void constructAndPopulate() {
        final DefaultConfigurer configurer = new DefaultConfigurer();
        configurer.setBuilderValue("k", "v");

        final Properties additionalSystemProperties = new Properties();
        additionalSystemProperties.setProperty("key1", "value");
        additionalSystemProperties.setProperty("key2", "value");
        configurer.setAdditionalSystemProperties(additionalSystemProperties);

        final Properties builderKeyToSystemPropertyNameMapping = new Properties();
        builderKeyToSystemPropertyNameMapping.setProperty("key1", "value");
        builderKeyToSystemPropertyNameMapping.setProperty("key2", "value");
        builderKeyToSystemPropertyNameMapping.setProperty("key3", "value");
        configurer.setBuilderKeyToSystemPropertyNameMappings(builderKeyToSystemPropertyNameMapping);

        assertThat(configurer.getBuilderKeysAndValues(), notNullValue());
        assertThat(configurer.getBuilderKeysAndValues().size(), is(1));

        assertThat(configurer.getAdditionalSystemProperties(), notNullValue());
        assertThat(configurer.getAdditionalSystemProperties().size(), is(additionalSystemProperties.size()));

        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings(), notNullValue());
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings().size(),
                is(builderKeyToSystemPropertyNameMapping.size()));
    }

    @Test
    public void configurerUsePublicGettersWhenNoValues() {
        final DefaultConfigurer configurer = getEmptyConfigurer();

        assertThat(configurer.getWkaAddress(), nullValue());
        assertThat(configurer.getExtendAddress(), nullValue());
        assertThat(configurer.getAppConsoleClassName(), nullValue());

        try {
            configurer.getWkaPort();

            fail("No number should have been set");
        } catch (NumberFormatException e) {
            // This is expected
        }

        try {
            configurer.getExtendPort();

            fail("No number should have been set");
        } catch (NumberFormatException e) {
            // This is expected
        }
    }

    @Test
    public void configurerUsePublicGettersWhenHasValues() {
        final String clusterNameKey = "ClusterName";
        final String wkaAddressKey = "WkaAddress";
        final String wkaPortKey = "WkaPort";
        final String extendPortKey = "ExtendPort";

        final String expectedClusterName = "MyCluster";
        final String expectedWkaAndExtendAddress = "1.2.3.4";
        final int expectedWkaPort = 12345;
        final int expectedExtendPort = 23456;

        final DefaultConfigurer configurer = new DefaultConfigurer();

        final Map<String, String> builderKeysAndValues = new HashMap<String, String>();
        builderKeysAndValues.put(clusterNameKey, expectedClusterName);
        builderKeysAndValues.put(wkaAddressKey, expectedWkaAndExtendAddress);
        builderKeysAndValues.put(wkaPortKey, Integer.toString(expectedWkaPort));
        builderKeysAndValues.put(extendPortKey, Integer.toString(expectedExtendPort));
        configurer.setBuilderValues(builderKeysAndValues);

        final Properties builderKeyToSystemPropertyNameMappings = new Properties();
        builderKeyToSystemPropertyNameMappings.setProperty(clusterNameKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(wkaAddressKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(wkaPortKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(extendPortKey, "any-name");
        configurer.setBuilderKeyToSystemPropertyNameMappings(builderKeyToSystemPropertyNameMappings);

        assertThat(configurer.getWkaAddress(), is(expectedWkaAndExtendAddress));
        assertThat(configurer.getExtendAddress(), is(expectedWkaAndExtendAddress));
        assertThat(configurer.getWkaPort(), is(expectedWkaPort));
        assertThat(configurer.getExtendPort(), is(expectedExtendPort));

        assertThat(configurer.getBuilderValueAsInt(wkaPortKey), is(expectedWkaPort));
        assertThat(configurer.getBuilderValueAsLong(wkaPortKey), is((long) expectedWkaPort));
        assertThat(configurer.getBuilderValueAsString(wkaPortKey), is(Integer.toString(expectedWkaPort)));
    }

    @Test
    public void equalsAndHashCodeWhenOtherIsThis() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final DefaultConfigurer otherConfigurer = thisConfigurer;

        assertThat(thisConfigurer.equals(otherConfigurer), is(true));
        assertThat(thisConfigurer.hashCode(), is(otherConfigurer.hashCode()));
    }

    @Test
    public void equalsAndHashCodeWhenOtherIsNull() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();

        assertThat(thisConfigurer.equals(null), is(false));
        assertThat(thisConfigurer.hashCode(), notNullValue());
    }

    @Test
    public void equalsAndHashCodeWhenOtherIsDifferentClass() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final String otherConfigurer = "really-a-string-and-not-a-configurer";

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
        assertThat(thisConfigurer.hashCode(), not(otherConfigurer.hashCode()));
    }

    @Test
    public void equalsAndHashCodeWhenOtherHasDifferentAdditionalSystemProperties() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final DefaultConfigurer otherConfigurer = getEmptyConfigurer();
        otherConfigurer.setAdditionalSystemProperty("k", "v");

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
    }

    @Test
    public void equalsAndHashCodeWhenOtherHasDifferentBuilderToSystemPropertyNameMappings() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final DefaultConfigurer otherConfigurer = getEmptyConfigurer();

        final Properties properties = new Properties();
        properties.setProperty("k", "v");

        otherConfigurer.setBuilderKeyToSystemPropertyNameMappings(properties);

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
        assertThat(thisConfigurer.hashCode(), not(otherConfigurer.hashCode()));
    }

    @Test
    public void equalsAndHashCodeWhenOtherHasDifferentBuilderKeyAndValues() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final DefaultConfigurer otherConfigurer = getEmptyConfigurer();
        otherConfigurer.setBuilderValue("k", "v");

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
        assertThat(thisConfigurer.hashCode(), not(otherConfigurer.hashCode()));
    }

    @Test
    public void equalsAndHashCodeWhenOtherIsSame() {
        final DefaultConfigurer thisConfigurer = getEmptyConfigurer();
        final DefaultConfigurer otherConfigurer = getEmptyConfigurer();

        assertThat(thisConfigurer.equals(otherConfigurer), is(true));
        assertThat(thisConfigurer.hashCode(), is(otherConfigurer.hashCode()));
    }

    @Test
    public void configurerToString() {
        assertThat(getEmptyConfigurer().toString().length() > 0, is(true));
    }

    private DefaultConfigurer getEmptyConfigurer() {
        return new DefaultConfigurer();
    }
}
