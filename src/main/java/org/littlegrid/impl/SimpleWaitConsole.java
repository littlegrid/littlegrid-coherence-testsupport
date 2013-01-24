package org.littlegrid.impl;

import java.io.IOException;

/**
 * Simple console that pauses.
 */
public class SimpleWaitConsole {
    public static void main(String[] args)
            throws IOException {

        System.out.println();
        System.out.println("Cluster member group launched, press Enter to shutdown or Ctrl+C to kill the process");
        System.in.read();
    }
}
