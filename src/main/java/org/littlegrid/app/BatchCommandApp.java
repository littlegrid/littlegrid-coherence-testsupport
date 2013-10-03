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

import com.tangosol.util.Resources;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import static java.lang.String.format;
import static org.littlegrid.app.CommandDslShell.Response;

/**
 * Batch command execution application.
 *
 * @since 2.15
 */
public class BatchCommandApp {
    private static final String COMMAND_FILE_ARGUMENT = "commandFile=";

    /**
     * Launches the application.
     *
     * @param args Arguments.
     * @throws IOException Indicates file not found.
     */
    public static void main(final String[] args)
            throws IOException {

        new BatchCommandApp().start(args);
    }

    Response start(final String[] args)
            throws IOException {

        final String commandFile = parseCommandFile(args);
        final InputStream in;

        if (commandFile.trim().length() == 0) {
            // No command file specified
            in = System.in;
        } else {
            final URL url = Resources.findFileOrResource(commandFile, BatchCommandApp.class.getClassLoader());

            if (url == null) {
                throw new FileNotFoundException(format("'%s' not found", commandFile));
            }

            in = url.openStream();
        }

        return new CommandDslShell(in, System.out).start(args);
    }

    private static String parseCommandFile(final String[] args) {
        for (final String argument : args) {
            if (argument.startsWith(COMMAND_FILE_ARGUMENT)) {
                return argument.replaceAll(COMMAND_FILE_ARGUMENT, "");
            }
        }

        return "";
    }
}
