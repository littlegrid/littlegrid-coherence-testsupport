/*
 * Copyright (c) 2010-2012 Jonathan Hall.
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

package org.littlegrid.features.additional_system_properties;

import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Additional system properties integration tests.
 */
public class AdditionalSystemPropertiesIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void additionalSystemProperties() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setAdditionalSystemProperties("properties/additionalSystemProperties.properties")
                .buildAndConfigureForStorageDisabledClient();

        assertThat(System.getProperty("SystemPropertyThatShouldHaveBeenSet"), notNullValue());
    }

    @Test
    public void additionalSystemProperty() {
        final String expectedKeyForString = "additionalSystemPropertyKeyForString";
        final String expectedValueForString = "additionalSystemPropertyValueForString";
        final String expectedKeyForInt = "additionalSystemPropertyKeyForInt";
        final int expectedValueForInt = 1;
        final String expectedKeyForBoolean = "additionalSystemPropertyKeyForBoolean";
        final boolean expectedValueForBoolean = true;

        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setAdditionalSystemProperty(expectedKeyForString, expectedValueForString)
                .setAdditionalSystemProperty(expectedKeyForInt, expectedValueForInt)
                .setAdditionalSystemProperty(expectedKeyForBoolean, expectedValueForBoolean)
                .buildAndConfigureForStorageDisabledClient();

        assertThat(System.getProperty(expectedKeyForString), is(expectedValueForString));
        assertThat(Integer.valueOf(System.getProperty(expectedKeyForInt)), is(expectedValueForInt));
        assertThat(Boolean.valueOf(System.getProperty(expectedKeyForBoolean)), is(expectedValueForBoolean));
    }
}
