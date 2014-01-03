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

import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;

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
import static org.littlegrid.ClusterMemberGroup.Configurer;

/**
 * Configurer tests.
 */
public class ImmutableConfigurerTest {
    @Test
    public void constructWithNoEntries() {
        final ImmutableConfigurer configurer = getEmptyImmutableConfigurer();

        assertThat(configurer.getBuilderKeysAndValues().size(), is(0));
        assertThat(configurer.getAdditionalSystemProperties().size(), is(0));
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings().size(), is(0));
        assertThat(configurer.getDirectMutableAccessToBuilderKeysAndValues().size(), is(0));
        assertThat(configurer.getDirectMutableAccessToAdditionalSystemProperties().size(), is(0));
        assertThat(configurer.getDirectMutableAccessToBuilderKeyToSystemPropertyNameMappings().size(), is(0));
    }

    @Test
    public void constructWithEntries() {
        final Map<String, String> builderKeysAndValues = singletonMap("key", "value");
        final Properties additionalSystemProperties = new Properties();
        additionalSystemProperties.setProperty("key1", "value");
        additionalSystemProperties.setProperty("key2", "value");

        final Properties builderKeyToSystemPropertyNameMapping = new Properties();
        builderKeyToSystemPropertyNameMapping.setProperty("key1", "value");
        builderKeyToSystemPropertyNameMapping.setProperty("key2", "value");
        builderKeyToSystemPropertyNameMapping.setProperty("key3", "value");

        final ImmutableConfigurer configurer = new ImmutableConfigurer(
                builderKeysAndValues, additionalSystemProperties, builderKeyToSystemPropertyNameMapping);

        assertThat(configurer.getBuilderKeysAndValues(), notNullValue());
        assertThat(configurer.getBuilderKeysAndValues().size(), is(builderKeysAndValues.size()));

        assertThat(configurer.getAdditionalSystemProperties(), notNullValue());
        assertThat(configurer.getAdditionalSystemProperties().size(), is(additionalSystemProperties.size()));

        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings(), notNullValue());
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMappings().size(),
                is(builderKeyToSystemPropertyNameMapping.size()));

        assertThat(configurer.getDirectMutableAccessToBuilderKeysAndValues().size(), is(builderKeysAndValues.size()));
        assertThat(configurer.getDirectMutableAccessToAdditionalSystemProperties().size(),
                is(additionalSystemProperties.size()));

        assertThat(configurer.getDirectMutableAccessToBuilderKeyToSystemPropertyNameMappings().size(),
                is(builderKeyToSystemPropertyNameMapping.size()));
    }

    @Test
    public void configurerUsePublicGettersWhenNoValues() {
        final Configurer configurer = getEmptyImmutableConfigurer();

        assertThat(configurer.getClusterName(), nullValue());
        assertThat(configurer.getWkaAddress(), nullValue());
        assertThat(configurer.getExtendAddress(), nullValue());

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

        final Map<String, String> builderKeysAndValues = new HashMap<String, String>();
        builderKeysAndValues.put(clusterNameKey, expectedClusterName);
        builderKeysAndValues.put(wkaAddressKey, expectedWkaAndExtendAddress);
        builderKeysAndValues.put(wkaPortKey, Integer.toString(expectedWkaPort));
        builderKeysAndValues.put(extendPortKey, Integer.toString(expectedExtendPort));

        final Properties builderKeyToSystemPropertyNameMappings = new Properties();
        builderKeyToSystemPropertyNameMappings.setProperty(clusterNameKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(wkaAddressKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(wkaPortKey, "any-name");
        builderKeyToSystemPropertyNameMappings.setProperty(extendPortKey, "any-name");

        final Configurer configurer = new ImmutableConfigurer(
                builderKeysAndValues, builderKeyToSystemPropertyNameMappings, new Properties());

        assertThat(configurer.getClusterName(), is(expectedClusterName));
        assertThat(configurer.getWkaAddress(), is(expectedWkaAndExtendAddress));
        assertThat(configurer.getExtendAddress(), is(expectedWkaAndExtendAddress));
        assertThat(configurer.getWkaPort(), is(expectedWkaPort));
        assertThat(configurer.getExtendPort(), is(expectedExtendPort));

        final ImmutableConfigurer immutableConfigurer = (ImmutableConfigurer) configurer;
        assertThat(immutableConfigurer.getBuilderValueAsInt(wkaPortKey), is(expectedWkaPort));
        assertThat(immutableConfigurer.getBuilderValueAsLong(wkaPortKey), is((long) expectedWkaPort));
        assertThat(immutableConfigurer.getBuilderValueAsString(wkaPortKey), is(Integer.toString(expectedWkaPort)));
    }

    @Test
    public void equalsWhenOtherIsThis() {
        final Configurer builder = getEmptyImmutableConfigurer();

        assertThat(builder.equals(builder), is(true));
    }

    @Test
    public void equalsWhenOtherIsNull() {
        assertThat(getEmptyImmutableConfigurer().equals(null), is(false));
    }

    @Test
    public void equalsWhenOtherIsDifferentClass() {
        assertThat(getEmptyImmutableConfigurer().equals("a-string"), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentAdditionalSystemProperties() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final ImmutableConfigurer otherConfigurer = getEmptyImmutableConfigurer();
        otherConfigurer.getDirectMutableAccessToAdditionalSystemProperties()
                .setProperty("hasAdditionalSystemPropertyToBeDifferent", "true");

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentBuilderToSystemPropertyNameMappings() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final ImmutableConfigurer otherConfigurer = getEmptyImmutableConfigurer();

        otherConfigurer.getDirectMutableAccessToBuilderKeyToSystemPropertyNameMappings()
                .setProperty(BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY,
                        "directory/example-littlegrid-builder-system-property-mapping-override.properties");

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
    }

    @Test
    public void equalsWhenOtherHasDifferentBuilderKeyAndValues() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final ImmutableConfigurer otherConfigurer = getEmptyImmutableConfigurer();
        otherConfigurer.getDirectMutableAccessToBuilderKeysAndValues().put("WkaPort", "1234");

        assertThat(thisConfigurer.equals(otherConfigurer), is(false));
    }

    @Test
    public void equalsWhenOtherIsSame() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final Configurer otherConfigurer = getEmptyImmutableConfigurer();

        assertThat(thisConfigurer.equals(otherConfigurer), is(true));
    }

    @Test
    public void hashCodeWhenOtherIsDifferent() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final ImmutableConfigurer otherConfigurer = getEmptyImmutableConfigurer();
        otherConfigurer.getDirectMutableAccessToBuilderKeysAndValues().put("WkaPort", "1234");

        assertThat(otherConfigurer.hashCode(), not(thisConfigurer.hashCode()));
    }

    @Test
    public void hashCodeWhenOtherIsSame() {
        final Configurer thisConfigurer = getEmptyImmutableConfigurer();
        final Configurer otherConfigurer = getEmptyImmutableConfigurer();

        assertThat(otherConfigurer.hashCode(), is(thisConfigurer.hashCode()));
    }

    @Test
    public void configurerToString() {
        assertThat(getEmptyImmutableConfigurer().toString().length() > 0, is(true));
    }

    private ImmutableConfigurer getEmptyImmutableConfigurer() {
        return new ImmutableConfigurer(new HashMap<String, String>(), new Properties(), new Properties());
    }
}
