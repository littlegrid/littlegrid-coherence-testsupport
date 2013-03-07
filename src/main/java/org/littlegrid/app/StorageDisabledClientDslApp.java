package org.littlegrid.app;

import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 * Simple storage disabled client DSL application, enabling control of a cluster member group
 * and access to CohQL.
 *
 * @since 2.15
 */
public class StorageDisabledClientDslApp {
    /**
     * Launches the application.
     *
     * @param args Arguments.
     */
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                STORAGE_DISABLED_CLIENT.name());

        final CommandDslShell shell = new CommandDslShell(System.in, System.out);
        shell.start("");
    }
}
