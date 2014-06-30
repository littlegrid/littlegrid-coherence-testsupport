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

package org.littlegrid.app;

import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

import static java.lang.String.format;
import static org.littlegrid.app.CommandDslShell.COMMAND_EXCEPTION;
import static org.littlegrid.app.CommandDslShell.COMMAND_UNKNOWN;
import static org.littlegrid.app.CommandDslShell.DefaultInput;
import static org.littlegrid.app.CommandDslShell.DefaultOutput;
import static org.littlegrid.app.Shell.Output;

/**
 * Socket application that accepts commands and processes them as per the
 * command DSL shell.
 *
 * @since 2.16
 */
public class SocketCommandApp {
    private static final String APP_SYSTEM_PROPERTY_PREFIX_KEY = "littlegrid.app.";
    private static final String PORT_SYSTEM_PROPERTY_KEY = APP_SYSTEM_PROPERTY_PREFIX_KEY + "Port";
    private static final String SHUTDOWN_PORT_SYSTEM_PROPERTY_KEY = APP_SYSTEM_PROPERTY_PREFIX_KEY + "ShutdownPort";
    private static final int DEFAULT_PORT = 21001;
    private static final String NAME = "Socket command application";

    /**
     * Launches the server.
     *
     * @param args Arguments.
     */
    public static void main(final String[] args) {
        new SocketCommandApp().start(args);
    }

    void start(final String[] args) {
        final int port = parsePort(args);

        System.out.println(format("%s ready - access port %d to use littlegrid DSL shell remotely",
                NAME, port));

        ServerSocket clientServerSocket = null;
        ServerSocket shutdownServerSocket = null;
        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newBuilder()
                .buildAndConfigure();

        try {
            clientServerSocket = new ServerSocket(port);
            shutdownServerSocket = new ServerSocket(port + 1);

            new Thread(new ShutdownConnectionListener(memberGroup, shutdownServerSocket)).start();
            new Thread(new ClientWhatever(memberGroup, clientServerSocket)).start();

            final CommandDslShell shell =
                    new CommandDslShell(new DefaultInput(System.in),
                            new DefaultOutput(System.out), memberGroup, null);
            shell.start(args);

            System.out.println(format("%s shutting down", NAME));
            System.exit(0);
        } catch (IOException e) {
            System.out.println(format("Could not listen on port: %d due to exception: %s", port, e));
        } finally {
            closeServerSocket(clientServerSocket);
            closeServerSocket(shutdownServerSocket);
        }
    }

    private static int parsePort(final String[] args) {
        for (final String argument : args) {
            if (argument.startsWith(PORT_SYSTEM_PROPERTY_KEY)) {
                final String portString = argument.replaceAll(PORT_SYSTEM_PROPERTY_KEY, "");

                return Integer.parseInt(portString);
            }
        }

        return DEFAULT_PORT;
    }

    private static void closeServerSocket(final ServerSocket serverSocket) {
        if (serverSocket != null) {
            try {
                serverSocket.close();
            } catch (IOException e) {
                // Ignore, can't do anything about the exception
            }
        }
    }

    private static void closeClientSocket(final Socket clientSocket) {
        if (clientSocket != null) {
            try {
                clientSocket.close();
            } catch (IOException e) {
                // Ignore, can't do anything about the exception
            }
        }
    }

    private static Output getShellOutput(final Socket clientSocket)
            throws IOException {

        return new PrintStreamAndWriterShellOutput(System.out,
                new PrintWriter(clientSocket.getOutputStream(), true));
    }

    /**
     * Print steam and writer shell output, directs output to both an output stream and
     * a print writer, whilst interpreting the message to be be written and prefixing
     * a response code.
     *
     * @since 2.16
     */
    static class PrintStreamAndWriterShellOutput extends DefaultOutput {
        private final PrintWriter printWriter;

        /**
         * Constructor.
         *
         * @param printStream Print stream.
         * @param printWriter Print writer.
         */
        public PrintStreamAndWriterShellOutput(final PrintStream printStream,
                                               final PrintWriter printWriter) {

            super(printStream);
            this.printWriter = printWriter;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printResponse(final String message) {
            super.printResponse(message);

            whatever(message);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printlnResponse(final String message) {
            super.printlnResponse(message);

            whatever(message);
        }

        private void whatever(final String message) {
            int returnCode = 0;

            if (message.startsWith(COMMAND_EXCEPTION)) {
                returnCode = 2;
            } else if (message.startsWith(COMMAND_UNKNOWN)) {
                returnCode = 1;
            }

            printWriter.println(returnCode + ":" + message);
        }
    }

    static class ClientWhatever implements Runnable {
        private final ClusterMemberGroup memberGroup;
        private final ServerSocket serverSocket;

        public ClientWhatever(final ClusterMemberGroup memberGroup,
                              final ServerSocket serverSocket) {

            this.memberGroup = memberGroup;
            this.serverSocket = serverSocket;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void run() {
            Socket clientSocket = null;

            try {
                clientSocket = serverSocket.accept();

                System.out.println("Socket accessed - launching DSL shell");

                new CommandDslShell(
                        new DefaultInput(clientSocket.getInputStream()),
                        getShellOutput(clientSocket),
                        memberGroup,
                        new ArrayList<String>())
                        .start(new String[]{});

                System.out.println(format("%s shutting down", NAME));
            } catch (IOException e) {
                // Do nothing
            } finally {
                closeClientSocket(clientSocket);
            }
        }
    }

    static class ShutdownConnectionListener implements Runnable {
        private final ClusterMemberGroup memberGroup;
        private final ServerSocket serverSocket;

        public ShutdownConnectionListener(final ClusterMemberGroup memberGroup,
                                          final ServerSocket serverSocket) {

            this.memberGroup = memberGroup;
            this.serverSocket = serverSocket;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void run() {
            Socket clientSocket = null;

            try {
                clientSocket = serverSocket.accept();

                ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);

                System.out.println("Socket accessed - shutting down");

                System.exit(0);
            } catch (IOException e) {
                // Do nothing
            } finally {
                closeClientSocket(clientSocket);
            }
        }
    }
}
