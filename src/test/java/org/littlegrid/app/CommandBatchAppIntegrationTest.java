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

import org.junit.Test;

import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Command batch application integration tests.
 */
public class CommandBatchAppIntegrationTest {
    @Test(expected = UnsupportedOperationException.class)
    public void construct() {
        new CommandBatchApp();
    }

    @Test
    public void runMainWithCommandsArgumentOnly()
            throws IOException {

        CommandBatchApp.main(new String[]{
                "some-string-before",
                "commands=start storage enabled; start jmx monitor; # ; bye",
                "some-string-after"
        });
    }

    @Test
    public void runMainWithCommandFileAndNoCommandsArgument()
            throws IOException {

        CommandBatchApp.main(new String[]{
                "some-string-before",
                "commandFile=command-file.txt",
                "some-string-after"
        });
    }

    @Test
    public void runMainWithCommandsArgumentAndCommandFile()
            throws IOException {

        CommandBatchApp.main(new String[]{"commands=members", "commandFile=command-file.txt"});
    }

    @Test(expected = FileNotFoundException.class)
    public void runMainWithCommandFileThatDoesNotExist()
            throws IOException {

        CommandBatchApp.main(new String[]{"commandFile=file-that-does-not-exist.xml"});
    }
}
