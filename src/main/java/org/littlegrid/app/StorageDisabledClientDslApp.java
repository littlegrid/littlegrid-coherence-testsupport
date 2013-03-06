package org.littlegrid.app;

import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 */
public class StorageDisabledClientDslApp {
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "AppConsoleClassName",
                CommandDslShell.class.getName());

        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                STORAGE_DISABLED_CLIENT.name());

        ClusterMemberGroupUtils.main(args);
    }
}
