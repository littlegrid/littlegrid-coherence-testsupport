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

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.impl.DefaultConfigurer.CACHE_CONFIGURATION_KEY;

/**
 * Default configurer tests.
 */
public class DefaultConfigurerTest {
    @Test
    public void setAndGetBuilderValueAsInt() {
        final String key = "key";
        final int value = 123;

        final DefaultConfigurer context = new DefaultConfigurer();
        context.setBuilderValue(key, value);

        assertThat(context.getBuilderValueAsInt(key), is(value));
    }

    @Test
    public void setAndGetBuilderValueAsLong() {
        final String key = "key";
        final long value = 123L;

        final DefaultConfigurer context = new DefaultConfigurer();
        context.setBuilderValue(key, value);

        assertThat(context.getBuilderValueAsLong(key), is(value));
    }

    @Test
    public void setAndGetBuilderValueAsString() {
        final String key = "key";
        final String value = "123";

        final DefaultConfigurer context = new DefaultConfigurer();
        context.setBuilderValue(key, value);

        assertThat(context.getBuilderValueAsString(key), is(value));
    }

    @Test
    public void getBuilderValueAsStringWhenNoEntry() {
        final DefaultConfigurer context = new DefaultConfigurer();

        assertThat(context.getBuilderValueAsString("no-entry"), nullValue());
    }

    @Test
    public void setAndGetBuilderValueAsStringWhenValueIsInt() {
        final String key = "key";
        final String value = "123";

        final DefaultConfigurer context = new DefaultConfigurer();
        context.setBuilderValue(key, Integer.parseInt(value));

        assertThat(context.getBuilderValueAsString(key), is(value));
    }

    @Test(expected = IllegalStateException.class)
    public void getPropertyNameFromMappingWhenDoesNotExist() {
        final DefaultConfigurer context = new DefaultConfigurer();

        context.getPropertyNameFromMapping("UnknownKey");
    }

    @Test(expected = IllegalArgumentException.class)
    public void setPropertyWhenValidWhenKeyIsNull() {
        final DefaultConfigurer context = new DefaultConfigurer();

        context.setPropertyWhenValid(new Properties(), null, null);
    }

    @Test
    public void setPropertyWhenValidWhenValueIsNull() {
        final DefaultConfigurer context = new DefaultConfigurer();
        final Properties properties = new Properties();

        context.setPropertyWhenValid(properties, CACHE_CONFIGURATION_KEY, null);

        assertThat(properties.size(), is(0));
    }

    @Test
    public void setPropertyWhenValidWhenValueIsSpaces() {
        final DefaultConfigurer context = new DefaultConfigurer();
        final Properties properties = new Properties();

        context.setPropertyWhenValid(properties, CACHE_CONFIGURATION_KEY, "   ");

        assertThat(properties.size(), is(0));
    }
}
