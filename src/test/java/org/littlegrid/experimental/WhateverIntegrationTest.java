package org.littlegrid.experimental;

import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.impl.DefaultClusterMemberGroupBuilder;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.littlegrid.ClusterMemberGroup.Builder;

/**
 *
 */
@Ignore
public class WhateverIntegrationTest {
    private static final Map<String, Whatever> activeMemberGroups =
            new HashMap<String, Whatever>();

    @Test
    public void whatever() {
        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();

            memberGroup = newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();
        } finally {
            shutdownClusterMemberGroups(memberGroup);
        }
    }

    public Builder newBuilder() {
        return new ReferenceCountingClusterMemberGroupBuilder(activeMemberGroups);
    }

    public void shutdownClusterMemberGroups(final ClusterMemberGroup... clusterMemberGroups) {
        for (final ClusterMemberGroup memberGroup : clusterMemberGroups) {
            if (activeMemberGroups.containsKey(memberGroup)) {

            }
        }
    }

    public class Whatever {
        private ClusterMemberGroup memberGroup;
        private AtomicInteger counter = new AtomicInteger();

        public Whatever(final ClusterMemberGroup memberGroup) {
            this.memberGroup = memberGroup;
        }

        public int incrementAndGet() {
            return counter.incrementAndGet();
        }

        public int decrementAndGet() {
            return counter.decrementAndGet();
        }

        public ClusterMemberGroup getMemberGroup() {
            return memberGroup;
        }
    }

    public class ReferenceCountingClusterMemberGroupBuilder extends DefaultClusterMemberGroupBuilder {
        private final Map<String, Whatever> activeMemberGroups;

        public ReferenceCountingClusterMemberGroupBuilder(final Map<String, Whatever> activeMemberGroups) {
            this.activeMemberGroups = activeMemberGroups;
        }

        @Override
        protected ClusterMemberGroup build() {
            final String key = this.toString();
            Whatever whatever = activeMemberGroups.get(key);

            if (whatever == null) {
                System.out.println("About to really build - BUILD");

                final ClusterMemberGroup memberGroup = super.build();

                whatever = new Whatever(memberGroup);
                whatever.incrementAndGet();

                activeMemberGroups.put(key, whatever);

                return memberGroup;
            } else {
                System.out.println("About to just use an existing one - REUSE");

                whatever = activeMemberGroups.get(key);
                whatever.incrementAndGet();

                activeMemberGroups.put(key, whatever);

                return whatever.getMemberGroup();
            }
        }
    }

    @Override
    public int hashCode() {
        return super.hashCode();    //To change body of overridden methods use File | Settings | File Templates.
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);    //To change body of overridden methods use File | Settings | File Templates.
    }
}
