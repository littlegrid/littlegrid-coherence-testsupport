package org.littlegrid.app;

/**
 * Management REPL application.
 *
 * @since 2.16
 */
public class ManagementReplApp {
    public static void main(final String[] args) {
        System.setProperty("tangosol.coherence.tcmp.enabled", "false");

        final ManagementDslShell shell = new ManagementDslShell(System.in, System.out);
        shell.start(args);
    }
}
