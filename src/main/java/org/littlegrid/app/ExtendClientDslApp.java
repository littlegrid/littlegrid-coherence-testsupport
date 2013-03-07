package org.littlegrid.app;

import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.EXTEND_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 *
 * @since 2.15
 */
public class ExtendClientDslApp {
    /**
     * @param args Arguments.
     */
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                EXTEND_CLIENT.name());

        final Builder builder = ClusterMemberGroupUtils.newBuilder();

        if (builder.getClientCacheConfiguration() == null
                || builder.getClientCacheConfiguration().trim().length() == 0) {

            System.out.println(
                    "No ClientCacheConfiguration file specified, cannot configure for Extend client - exiting");
        } else {
            final CommandDslShell shell = new CommandDslShell(System.in, System.out);
            shell.start("");
        }
    }
}
