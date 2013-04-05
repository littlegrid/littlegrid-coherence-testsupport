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

package org.littlegrid.app;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Command DSL shell tests.
 */
public class CommandDslShellIntegrationTest {
    @Test
    public void stopMember() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=stop member 1; bye"});
    }

    @Test
    public void shutdownMember() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=shutdown member 1; bye"});
    }

    @Test
    public void stopAll() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=stop all; bye"});
    }

    @Test
    public void shutdownAll() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=shutdown all; bye"});
    }

    @Test
    public void bye() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=bye"});
    }

    @Test
    public void emptyString() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=;;bye"});
    }

    @Test
    public void quit() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=quit"});
    }

    @Test
    public void members() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=members; bye"});
    }

    @Test
    public void unknownCommand() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=this is unknown; bye"});
    }

    @Test
    public void invalidExtendPortCommand() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=start extend proxy ABC; bye"});
    }

    @Test
    public void sleep() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=sleep 1; bye"});
    }

    @Test
    public void startStorageEnabled() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=start storage enabled; bye"});
    }

    @Test
    public void startExtendProxy() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=start extend proxy 25001; bye"});
    }

    @Test
    public void startJmxMonitor() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=start jmx monitor; bye"});
    }

    @Test
    public void help() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=help; bye"});
    }

    @Test
    public void comment() {
        new CommandDslShell(System.in, System.out).start(new String[]{"commands=# comment; bye"});
    }

    @Test
    @Ignore
    public void cohQl() {

    }
}
