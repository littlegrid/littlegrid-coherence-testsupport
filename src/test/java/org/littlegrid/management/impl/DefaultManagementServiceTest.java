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

package org.littlegrid.management.impl;

import org.junit.Test;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.TabularResult;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Default management service tests.
 */
public class DefaultManagementServiceTest {
    private static final String ALIAS_NAME_COLUMN_NAME = "alias";
    private static final String ALIAS_VALUE_COLUMN_NAME = "value";

    @Test
    public void findAliasesWhenNoneExist() {
        final ManagementService service = new DefaultManagementService(null, new Properties(), null, null, null);

        assertThat(service.findAliases().getRowCount(), is(0));
    }

    @Test
    public void findAliasesWhenSomeDoExist() {
        final String aliasName = "some-alias-name";
        final Object aliasValue = "some-alias-value";

        final ManagementService service;

        {
            final Properties aliases = new Properties();
            aliases.setProperty(aliasName, aliasValue.toString());

            service = new DefaultManagementService(null, aliases, null, null, null);
        }

        final TabularResult aliases = service.findAliases();
        assertThat(aliases.getRowCount(), is(1));
        assertThat((String) aliases.getValue(ALIAS_NAME_COLUMN_NAME, 0), is(aliasName));
        assertThat(aliases.getValue(ALIAS_VALUE_COLUMN_NAME, 0), is(aliasValue));
    }
}
