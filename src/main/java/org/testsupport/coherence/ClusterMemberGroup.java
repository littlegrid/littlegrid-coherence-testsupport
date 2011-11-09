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
        ClusterMemberGroup build();

        Builder setCacheConfiguration(String cacheConfiguration);
        Builder setStorageEnabledSpecificCacheConfiguration(String cacheConfiguration);
        Builder setExtendProxySpecificCacheConfiguration(String cacheConfiguration);
        Builder setOverrideConfiguration(String overrideConfiguration);
        Builder setClientCacheConfiguration(String cacheConfiguration);
        Builder setClientOverrideConfiguration(String overrideConfiguration);

        Builder setSystemProperties(Properties properties);
        Builder setExtendProxySpecificSystemProperties(Properties properties);

        Builder setStorageEnabledCount(int numberOfMembers);
        int getStorageEnabledCount();

        Builder setStorageEnabledExtendProxyCount(int numberOfMembers);
        int getStorageEnabledExtendProxyCount();

        Builder setExtendProxyCount(int numberOfMembers);
        int getExtendProxyCount();

        Builder setLogLevel(int logLevel);
        int getLogLevel();

        Builder setClusterMemberInstanceClassName(String clusterMemberInstanceClassName);
        String getClusterMemberInstanceClassName();

        Builder setJarsToExcludeFromClassPath(String... jarsToExcludeFromClassPath);

        Builder setWkaAddress(String wkaAddress);
        String getWkaAddress();

        Builder setWkaPort(int wkaPort);
        int getWkaPort();

        Builder setBuilderProperties(Properties properties);
    }
}
