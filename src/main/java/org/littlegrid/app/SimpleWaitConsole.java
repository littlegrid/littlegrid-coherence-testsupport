package org.littlegrid.app;

import java.io.IOException;

/**
 * Simple console that pauses.
 *
 * @since 2.14
 */
public class SimpleWaitConsole {
    /**
     * Main method.
     *
     * @param args Arguments.
     * @throws IOException indicates an exception.
     */
    public static void main(final String[] args)
            throws IOException {

        System.out.println();
        System.out.println("Cluster member group launched, press Enter to shutdown or Ctrl+C to kill the process");
        System.in.read();
    }
}
