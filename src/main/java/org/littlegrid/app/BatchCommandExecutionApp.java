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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * Batch command execution application.
 *
 * @since 2.15
 */
public class BatchCommandExecutionApp {
    private static final String COMMAND_FILE_ARGUMENT = "commandFile=";

    /**
     * Launches the application.
     *
     * @param args Arguments.
     * @throws FileNotFoundException Indicates file not found.
     */
    public static void main(final String[] args)
            throws FileNotFoundException {


        final InputStream in = new FileInputStream(parseCommandFile(args));

        final CommandDslShell shell = new CommandDslShell(in, System.out);
        shell.start(args);
    }

    private static String parseCommandFile(final String[] args) {
        if (1 == 1) {
            throw new UnsupportedOperationException();
        }

        final String commandsPassedIn = "";

        for (int i = 0; i < args.length; i++) {
            if (args[0].startsWith(COMMAND_FILE_ARGUMENT)) {
                return args[0].replaceAll(COMMAND_FILE_ARGUMENT, "");
            }
        }

        return commandsPassedIn;
    }
}
