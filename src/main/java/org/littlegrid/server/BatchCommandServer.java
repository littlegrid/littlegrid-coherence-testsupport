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

package org.littlegrid.server;

/**
 * Batch command server to play with ideas about sending commands to littlegrid
 * via a socket.
 *
 * @since 2.16
 */
public class BatchCommandServer {
/*
    private static final String DEFAULT_SERVER_APP_PORT = "21001";
    private static final String SERVER_APP_PORT_SYSTEM_PROPERTY_KEY = "littlegrid.server.app.port";

    */
/**
     * Launches the application.
     *
     * @param args  Arguments.
     *//*

    public static void main(final String[] args) {
        final String portString = System.getProperty(SERVER_APP_PORT_SYSTEM_PROPERTY_KEY, DEFAULT_SERVER_APP_PORT);

        System.out.println("About to launch batch command server - access the socket to start littlegrid DSL shell");

        ServerSocket serverSocket = null;
        Socket clientSocket = null;

        try {
            serverSocket = new ServerSocket(Integer.parseInt(portString));
            clientSocket = serverSocket.accept();

            System.out.println("Socket accessed - launching DSL shell");

            new CommandDslShell(clientSocket.getInputStream(), getPrintStream(clientSocket))
                    .start(new String[]{});

            System.out.println("Batch command server shutting down");
        } catch (IOException e) {
            System.err.println(format("Could not listen on port: %s", portString));
        } finally {
            closeClientSocket(clientSocket);
            closeServerSocket(serverSocket);
        }
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

        return new PrintStreamAndWriterWrapper(System.out, new PrintWriter(clientSocket.getOutputStream(), true));
    }

    */
/**
     * Print steam and wrapper wrapper, directs output to both an output stream and
     * a print writer.
     *//*

    public static class PrintStreamAndWriterWrapper extends PrintStream {
        private final PrintWriter printWriter;

        */
/**
         * Constructor.
         *
         * @param outputStream Output stream.
         * @param printWriter  Print writer.
         *//*

        public PrintStreamAndWriterWrapper(final OutputStream outputStream,
                                           final PrintWriter printWriter) {

            super(outputStream);
            this.printWriter = printWriter;
        }

        */
/**
         * {@inheritDoc}
         *//*

        @Override
        public void print(String message) {
            super.print(message);
            printWriter.println(message);
        }
    }
*/
}
