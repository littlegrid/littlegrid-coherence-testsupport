package org.littlegrid.app;

import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.EXTEND_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 */
public class ExtendClientDslApp {
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "AppConsoleClassName",
                CommandDslShell.class.getName());

        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                EXTEND_CLIENT.name());

        final String clientCacheConfigurationFilename =
                ClusterMemberGroupUtils.newBuilder().getClientCacheConfiguration();

        if (clientCacheConfigurationFilename == null || clientCacheConfigurationFilename.length() == 0) {
            throw new IllegalStateException(
                    "Client cache configuration file cannot be null or blank when using Extend");
        }

        ClusterMemberGroupUtils.main(args);
    }
}
