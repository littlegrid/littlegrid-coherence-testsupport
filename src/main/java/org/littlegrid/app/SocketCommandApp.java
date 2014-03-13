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
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;

import static java.lang.String.format;
import static org.littlegrid.app.CommandDslShell.COMMAND_EXCEPTION;
import static org.littlegrid.app.CommandDslShell.COMMAND_UNKNOWN;

/**
 * Socket application that accepts commands and processes them as per the
 * command DSL shell.
 *
 * @since 2.16
 */
public class SocketCommandApp {
    private static final String PORT_ARGUMENT = "littlegrid.app.Port=";
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


        ServerSocket serverSocket = null;
        ServerSocket serverSocket2 = null;
        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newBuilder()
                .buildAndConfigure();

        try {
            serverSocket = new ServerSocket(port);
            serverSocket2 = new ServerSocket(port + 1);

            new Thread(new Whatever(memberGroup, serverSocket)).start();
            new Thread(new Whatever(memberGroup, serverSocket)).start();
            new Thread(new Whatever2(memberGroup, serverSocket2)).start();

            final CommandDslShell shell = new CommandDslShell(System.in, System.out, memberGroup, null, true);
            StorageDisabledClientReplApp.start(shell, args);

            System.out.println(format("%s shutting down", NAME));
        } catch (IOException e) {
            System.out.println(format("Could not listen on port: %d due to exception: %s", port, e));
        } finally {
            closeServerSocket(serverSocket);
            closeServerSocket(serverSocket2);
        }
    }

    private static int parsePort(final String[] args) {
        for (final String argument : args) {
            if (argument.startsWith(PORT_ARGUMENT)) {
                final String portString = argument.replaceAll(PORT_ARGUMENT, "");

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

    private static PrintStream getPrintStream(final Socket clientSocket)
            throws IOException {

        return new PrintStreamAndWriterMessageResponseInterpreterWrapper(System.out,
                new PrintWriter(clientSocket.getOutputStream(), true));
    }

    /**
     * Print steam and wrapper wrapper, directs output to both an output stream and
     * a print writer, whilst interpreting the message to be be written and prefixing
     * a response code.
     *
     * @since 2.16
     */
    static class PrintStreamAndWriterMessageResponseInterpreterWrapper extends PrintStream {
        private final PrintWriter printWriter;

        /**
         * Constructor.
         *
         * @param outputStream Output stream.
         * @param printWriter  Print writer.
         */
        public PrintStreamAndWriterMessageResponseInterpreterWrapper(final OutputStream outputStream,
                                                                     final PrintWriter printWriter) {

            super(outputStream);
            this.printWriter = printWriter;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void print(String message) {
            super.print(message);

            int returnCode = 0;

            if (message.startsWith(COMMAND_EXCEPTION)) {
                returnCode = 2;
            } else if (message.startsWith(COMMAND_UNKNOWN)) {
                returnCode = 1;
            }

            printWriter.println(returnCode + ":" + message);
        }
    }

    public static class Whatever implements Runnable {
        private final ClusterMemberGroup memberGroup;
        private final ServerSocket serverSocket;

        public Whatever(final ClusterMemberGroup memberGroup,
                        final ServerSocket serverSocket) {

            this.memberGroup = memberGroup;
            this.serverSocket = serverSocket;
        }

        @Override
        public void run() {
            Socket clientSocket = null;

            try {
                clientSocket = serverSocket.accept();

                System.out.println("Socket accessed - launching DSL shell");

                new CommandDslShell(clientSocket.getInputStream(),
                        getPrintStream(clientSocket),
                        memberGroup,
                        new ArrayList<String>(), false)
                        .start(new String[]{});

                System.out.println(format("%s shutting down", NAME));
            } catch (IOException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            } finally {
                closeClientSocket(clientSocket);
            }

        }
    }

    public static class Whatever2 implements Runnable {
        private final ClusterMemberGroup memberGroup;
        private final ServerSocket serverSocket;

        public Whatever2(final ClusterMemberGroup memberGroup,
                         final ServerSocket serverSocket) {

            this.memberGroup = memberGroup;
            this.serverSocket = serverSocket;

        }

        @Override
        public void run() {
            Socket clientSocket = null;

            try {
                clientSocket = serverSocket.accept();

                ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);

                System.out.println("Socket accessed - shutting down");

                System.exit(1);
            } catch (IOException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            } finally {
                closeClientSocket(clientSocket);
            }

        }
    }
}
