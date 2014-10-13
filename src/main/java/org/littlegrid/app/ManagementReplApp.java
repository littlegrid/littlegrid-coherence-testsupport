package org.littlegrid.app;

/**
 * Management REPL application.
 */
public class ManagementReplApp {
    public static void main(final String[] args) {
        final ManagementDslShell shell = new ManagementDslShell(System.in, System.out);
        shell.start(args);
    }
}
