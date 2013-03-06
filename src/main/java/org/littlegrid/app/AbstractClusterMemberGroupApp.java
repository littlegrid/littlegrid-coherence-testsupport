package org.littlegrid.app;

import com.tangosol.util.ClassHelper;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

/**
 * Created with IntelliJ IDEA.
 * User: jhall
 * Date: 06/03/13
 * Time: 18:48
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractClusterMemberGroupApp {
    public static void main(final String[] args) {
        final ClusterMemberGroup.Builder builder = ClusterMemberGroupUtils.newBuilder();
        final ClusterMemberGroup memberGroup;

        try {
            final Class consoleClass =
                    ClusterMemberGroupUtils.class.getClassLoader().loadClass(builder.getAppConsoleClassName());

            memberGroup = builder.buildAndConfigure();

            ClassHelper.invokeStatic(consoleClass, "main", new Object[]{new String[]{}});
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    public abstract static class AbstractClusterMemberGroupAwareShell {

    }
}
