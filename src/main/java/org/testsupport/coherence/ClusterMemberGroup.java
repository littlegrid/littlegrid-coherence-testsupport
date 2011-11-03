package org.testsupport.coherence;

import java.net.URL;
import java.util.List;
import java.util.Properties;

/**
 * Cluster member group, a collection of cluster members enabling them to be controlled as a group or
 * individually for some operations.
 */
public interface ClusterMemberGroup {
    /**
     * Starts all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup startAll();

    /**
     * Shuts down specific cluster members in the group.
     *
     * @param memberIds Ids of cluster member(s) to shutdown.
     * @return member group.
     */
    ClusterMemberGroup shutdownMember(int... memberIds);

    /**
     * Shuts down all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup shutdownAll();

    /**
     * Stops specific cluster members.
     *
     * @param memberIds Ids of cluster member(s) to stop.
     * @return member group.
     */
    ClusterMemberGroup stopMember(int... memberIds);

    /**
     * Stops all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup stopAll();

    /**
     * Returns a specific cluster member.
     *
     * @param memberId Member id
     * @return member or <b>null</b> if called on a group that hasn't or can't start-up.
     */
    ClusterMember getClusterMember(int memberId);

    /**
     * Returns the member Ids of started cluster members.
     *
     * @return members Ids.
     */
    List<Integer> getStartedMemberIds();

    /**
     * Builder interface for cluster member group.
     */
    static interface Builder {
        enum Topology {
            STORAGE_ENABLED_ONLY,
            COMPOSITE_STORAGE_ENABLED_PROXY,
            SEPARATE_PROXY_AND_STORAGE_ENABLED,
            EXTEND_PROXY_ONLY
        }

        ClusterMemberGroup build();

        Builder setCacheConfiguration(String cacheConfiguration);

        Builder setClientCacheConfiguration(String cacheConfiguration);

        Builder setSystemProperties(Properties properties);

        Builder setTopology(Topology topology);

        Builder setNumberOfMembers(int numberOfMembers);

        Builder setClusterMemberClassName(String clusterMemberClassName);

        Builder setClassPath(URL[] classPathUrls);

        Builder setJarsToExcludeFromClassPath(String... jarsToExcludeFromClassPathUrls);

        Builder setWkaPort(int wkaPort);
    }
}
