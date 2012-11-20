package org.littlegrid.experimental;

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
//@Ignore
public class Holder2IntegrationTest {
    private static final Map<String, CountingClusterMemberGroupHolder> activeMemberGroups =
            new HashMap<String, CountingClusterMemberGroupHolder>();

    @Test
    public void whatever() {
        ClusterMemberGroup memberGroup1 = null;
        ClusterMemberGroup memberGroup2 = null;
        ClusterMemberGroup memberGroup3 = null;
        ClusterMemberGroup memberGroup4 = null;

        try {
            memberGroup1 = newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();

            memberGroup2 = newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();

            memberGroup3 = newBuilder()
                    .setStorageEnabledCount(1)
                    .buildAndConfigureForStorageDisabledClient();

            memberGroup4 = newBuilder()
                    .setStorageEnabledCount(1)
                    .setExtendProxyCount(1)
                    .buildAndConfigureForStorageDisabledClient();
        } finally {
            shutdownClusterMemberGroups(memberGroup1, memberGroup2, memberGroup3, memberGroup4);
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

    public class CountingClusterMemberGroupHolder {
        private ClusterMemberGroup memberGroup;
        private AtomicInteger counter = new AtomicInteger();

        public CountingClusterMemberGroupHolder(final ClusterMemberGroup memberGroup) {
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
        private final Map<String, CountingClusterMemberGroupHolder> activeMemberGroups;

        public ReferenceCountingClusterMemberGroupBuilder(final Map<String, CountingClusterMemberGroupHolder> activeMemberGroups) {
            this.activeMemberGroups = activeMemberGroups;
        }

        @Override
        protected ClusterMemberGroup build() {
            final String key = this.toString();
            CountingClusterMemberGroupHolder countingClusterMemberGroupHolder = activeMemberGroups.get(key);

            if (countingClusterMemberGroupHolder == null) {
                System.out.println("About to really build - BUILD");

                final ClusterMemberGroup memberGroup = super.build();

                countingClusterMemberGroupHolder = new CountingClusterMemberGroupHolder(memberGroup);
                countingClusterMemberGroupHolder.incrementAndGet();

                activeMemberGroups.put(key, countingClusterMemberGroupHolder);

                return memberGroup;
            } else {
                System.out.println("About to just use an existing one - REUSE");

                countingClusterMemberGroupHolder = activeMemberGroups.get(key);
                countingClusterMemberGroupHolder.incrementAndGet();

                activeMemberGroups.put(key, countingClusterMemberGroupHolder);

                return countingClusterMemberGroupHolder.getMemberGroup();
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
