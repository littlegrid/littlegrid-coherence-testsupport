package org.testsupport.coherence.impl;

import java.net.URL;

/**
 * Cluster member group configuration, used to configure the number of members and their features within
 * the cluster.
 */
@Deprecated
class ClusterMemberGroupConfig {
    public static final int DEFAULT_NUMBER_OF_CLUSTER_MEMBERS = 1;
    public static final int DEFAULT_THREADS_IN_START_UP_POOL = 5;
    public static final String DEFAULT_CLUSTER_MEMBER_IMPLEMENTATION =
            "org.testsupport.coherence.impl.DefaultClusterMemberDelegateImpl";

    private int numberOfClusterMembers = DEFAULT_NUMBER_OF_CLUSTER_MEMBERS;
    private int numberOfThreadsInStartUpPool = DEFAULT_THREADS_IN_START_UP_POOL;
    private String clusterMemberClassName = DEFAULT_CLUSTER_MEMBER_IMPLEMENTATION;
    private URL[] classPathUrls;
    private String[] jarsToExcludeFromClassPathUrls;

    public ClusterMemberGroupConfig() {
    }

    public ClusterMemberGroupConfig(String... jarsToExcludeFromClassPathUrls) {
        this.jarsToExcludeFromClassPathUrls = jarsToExcludeFromClassPathUrls;
    }

    public ClusterMemberGroupConfig(URL[] classPathUrls) {
        this.classPathUrls = classPathUrls;
    }

    public int getNumberOfClusterMembers() {
        return numberOfClusterMembers;
    }

    public void setNumberOfClusterMembers(int numberOfClusterMembers) {
        this.numberOfClusterMembers = numberOfClusterMembers;
    }

    public int getNumberOfThreadsInStartUpPool() {
        return numberOfThreadsInStartUpPool;
    }

    public void setNumberOfThreadsInStartUpPool(int numberOfThreadsInStartUpPool) {
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;
    }

    public URL[] getClassPathUrls() {
        return classPathUrls;
    }

    public void setClassPathUrls(URL[] classPathUrls) {
        this.classPathUrls = classPathUrls;
    }

    public String getClusterMemberClassName() {
        return clusterMemberClassName;
    }

    public void setClusterMemberClassName(String clusterMemberClassName) {
        this.clusterMemberClassName = clusterMemberClassName;
    }

    public String[] getJarsToExcludeFromClassPathUrls() {
        return jarsToExcludeFromClassPathUrls;
    }

    public void setJarsToExcludeFromClassPathUrls(String... jarsToExcludeFromClassPathUrls) {
        this.jarsToExcludeFromClassPathUrls = jarsToExcludeFromClassPathUrls;
    }
}
