package org.littlegrid.app;

import com.tangosol.net.CacheFactory;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.ClusterMemberGroup.BuildAndConfigureEnum.STORAGE_DISABLED_CLIENT;
import static org.littlegrid.ClusterMemberGroup.Builder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 */
public class StorageDisabledCacheFactoryApp {
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "AppConsoleClassName",
                CacheFactory.class.getName());

        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                STORAGE_DISABLED_CLIENT.name());

        ClusterMemberGroupUtils.main(args);
    }
}
