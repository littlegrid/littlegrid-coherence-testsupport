package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.ClusterMemberGroupFactory;

import java.util.Properties;

/**
 */
public class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private String cacheConfiguration;
    private int storageEnabledCount = 1;
    private Properties systemProperties;
    private ClusterMemberGroupConfig memberGroupConfig;

    @Override
    public ClusterMemberGroup build() {
        return ClusterMemberGroupFactory.createCacheServerGroup(storageEnabledCount, cacheConfiguration,
                systemProperties, memberGroupConfig, false);
    }

    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setSystemProperties(final Properties properties) {
        this.systemProperties = properties;

        return this;
    }

    @Override
    public ClusterMemberGroup.Builder setTopology(Topology topology) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ClusterMemberGroup.Builder setMemberCount(int numberOfMembers) {
        throw new UnsupportedOperationException();
    }
}
