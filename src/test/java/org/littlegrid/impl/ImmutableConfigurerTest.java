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

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Configurer tests.
 */
public class ImmutableConfigurerTest {
    @Test
    public void constructWithNoEntries() {
        final ImmutableConfigurer configurer = new ImmutableConfigurer(
                new HashMap<String, String>(), new Properties(), new Properties());

        assertThat(configurer.getBuilderKeysAndValues().size(), is(0));
        assertThat(configurer.getAdditionalSystemProperties().size(), is(0));
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMapping().size(), is(0));
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

        assertThat(configurer.getBuilderKeysAndValues().size(), is(builderKeysAndValues.size()));
        assertThat(configurer.getAdditionalSystemProperties().size(), is(additionalSystemProperties.size()));
        assertThat(configurer.getBuilderKeyToSystemPropertyNameMapping().size(),
                is(builderKeyToSystemPropertyNameMapping.size()));
    }
}
