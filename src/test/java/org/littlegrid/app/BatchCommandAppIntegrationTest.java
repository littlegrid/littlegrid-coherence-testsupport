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

import org.junit.Test;

import java.io.FileNotFoundException;
import java.io.IOException;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.app.CommandDslShell.Response;

/**
 * Batch command application integration tests.
 */
public class BatchCommandAppIntegrationTest {
    @Test
    public void startCommandFileCommandsOnly()
            throws IOException {

        final Response response = new BatchCommandApp()
                .start(new String[]{"commandFile=command-file.txt"});

        assertThat(response.getValidCommandsExecuted(), is(1));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(1));
        assertThat(response.isExitRequested(), is(true));
    }

    @Test
    public void startCommandsAndCommandFile()
            throws IOException {

        final Response response = new BatchCommandApp()
                .start(new String[]{"commands=stop member 1; # about to process file", "commandFile=command-file.txt"});

        assertThat(response.getValidCommandsExecuted(), is(2));
        assertThat(response.getInvalidCommandsExecuted(), is(0));
        assertThat(response.getUnknownCommandsExecuted(), is(0));
        assertThat(response.getCommentCommandsExecuted(), is(2));
        assertThat(response.isExitRequested(), is(true));
    }


    @Test
    public void runMainWithCommandsArgumentOnly()
            throws IOException {

        BatchCommandApp.main(new String[]{
                "some-string-before",
                "commands=start storage enabled; start jmx monitor; # ; bye",
                "some-string-after"
        });
    }

    @Test
    public void runMainWithCommandFileAndNoCommandsArgument()
            throws IOException {

        BatchCommandApp.main(new String[]{
                "some-string-before",
                "commandFile=command-file.txt",
                "some-string-after"
        });
    }

    @Test
    public void runMainWithCommandsArgumentAndCommandFile()
            throws IOException {

        BatchCommandApp.main(new String[]{"commands=members", "commandFile=command-file.txt"});
    }

    @Test(expected = FileNotFoundException.class)
    public void runMainWithCommandFileThatDoesNotExist()
            throws IOException {

        BatchCommandApp.main(new String[]{"commandFile=file-that-does-not-exist.xml"});
    }
}
