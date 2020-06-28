/*
 * Copyright (c) 2010-2020 Jonathan Hall.
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

package org.littlegrid.app;

import com.tangosol.net.CacheFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.littlegrid.support.SystemUtils;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.GregorianCalendar;
import java.util.Properties;

import static java.lang.String.format;
import static java.util.Calendar.HOUR_OF_DAY;
import static java.util.Calendar.MINUTE;
import static java.util.Calendar.SECOND;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroupBuilder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;
import static org.littlegrid.app.CommandDslShell.Response;

/**
 * Command DSL shell integration tests.
 */
public class CommandDslShellIntegrationTest {
    private Properties systemProperties;

    @Before
    public void beforeTest() {
        systemProperties = SystemUtils.snapshotSystemProperties();

        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                CONFIGURE_FOR_STORAGE_DISABLED_CLIENT.name());
    }

    @After
    public void afterTest() {
        System.setProperties(systemProperties);
    }

    @Test
    public void stopMember() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=stop member 1; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void stopMembers() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=stop member 1 2 3; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void shutdownMember() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=shutdown member 1; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void stopAll() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=stop all; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void shutdownAll() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=shutdown all; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void bye() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void emptyString() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=;;bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void quit() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=quit"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void members() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=members; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void unknownCommand() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=this is unknown; bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(1));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void invalidExtendPortCommand() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=start extend proxy ABC; bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(1));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void sleep() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=sleep 1; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void sleepUntilWithValidTime() {
        final GregorianCalendar now = new GregorianCalendar();
        final int hour = now.get(HOUR_OF_DAY);
        final int minutes = now.get(MINUTE);
        final int seconds = now.get(SECOND);

        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{format("commands=sleep until %d:%d:%d; bye",
                        hour, minutes, seconds)});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void sleepUntilWithInvalidTime() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=sleep until 2a:0b:00; bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(1));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void sleepUntilWithInvalidTimeThatHasTooManyDelimiters() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=sleep until 20:00:00:00; bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(1));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void startStorageEnabled() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=start storage enabled; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void startMultipleStorageEnabled() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=start storage enabled * 2; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void startExtendProxy() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=start extend proxy 25001; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void startJmxMonitor() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=start jmx monitor; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void help() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=help; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void comment() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=# comment; # this is another comment; bye"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(2));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void outputDate() {
        final Response response = new CommandDslShell(System.in, System.out)
                .start(new String[]{"commands=date; bye"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void setSite() {
        final String site = "mysite";

        final CommandDslShell shell = new CommandDslShell(System.in, System.out);
        final Response response = shell.start(new String[]{format("commands=site = %s; bye", site)});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));

        assertThat(shell.getSite(), is(site));
        assertThat(shell.getRack(), nullValue());
        assertThat(shell.getMachine(), nullValue());
    }

    @Test
    public void setRack() {
        final String rack = "myrack";

        final CommandDslShell shell = new CommandDslShell(System.in, System.out);
        final Response response = shell.start(new String[]{format("commands=rack = %s; bye", rack)});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));

        assertThat(shell.getSite(), nullValue());
        assertThat(shell.getRack(), is(rack));
        assertThat(shell.getMachine(), nullValue());
    }

    @Test
    public void setMachine() {
        final String machine = "mymachine";

        final CommandDslShell shell = new CommandDslShell(System.in, System.out);
        final Response response = shell.start(new String[]{format("commands=machine = %s; bye", machine)});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(0));
        assertThat(response.isExitRequested(), is(true));

        assertThat(shell.getSite(), nullValue());
        assertThat(shell.getRack(), nullValue());
        assertThat(shell.getMachine(), is(machine));
    }


    @Test
    public void cohQl() {
        final InputStream originalInput = System.in;

        try {
            System.setIn(new ByteArrayInputStream(new byte[]{}));

            final Response response = new CommandDslShell(System.in, System.out)
                    .start(new String[]{"commands=cohql; bye"});

            if (CacheFactory.VERSION.startsWith("3.5")) {
                assertThat(response.getValidCommandsExecuted(), is(1));
                assertThat(response.getInvalidCommandsExecuted(), is(1));
            } else {
                assertThat(response.getValidCommandsExecuted(), is(2));
                assertThat(response.getInvalidCommandsExecuted(), is(0));
            }

            assertThat(response.getUnknownCommandsExecuted(), is(0));
            assertThat(response.getCommentCommandsExecuted(), is(0));
            assertThat(response.isExitRequested(), is(true));
        } finally {
            System.setIn(originalInput);
        }
    }

    @Test
    public void console() {
        final InputStream originalInput = System.in;

        try {
            System.setIn(new ByteArrayInputStream("bye".getBytes()));

            final Response response = new CommandDslShell(System.in, System.out)
                    .start(new String[]{"commands=console; bye"});

            assertThat(response.getValidCommandsExecuted(), is(2));
            assertThat(response.getInvalidCommandsExecuted(), is(0));
            assertThat(response.getUnknownCommandsExecuted(), is(0));
            assertThat(response.getCommentCommandsExecuted(), is(0));
            assertThat(response.isExitRequested(), is(true));
        } finally {
            System.setIn(originalInput);
        }
    }
}
