package org.littlegrid.coherence.impl;

import org.junit.Test;
import org.littlegrid.coherence.ClusterMember;
import org.littlegrid.coherence.ClusterMemberGroup;
import org.littlegrid.coherence.ClusterMemberGroupUtils;
import org.littlegrid.coherence.impl.ChildFirstUrlClassLoader;
import org.littlegrid.common.AbstractTest;

import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

/**
 * Cluster member tests.
 */
public class ClusterMemberClassLoaderTest extends AbstractTest {
    @Test
    public void getClassLoader() {
        final int numberOfMembers = 3;
        ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(numberOfMembers).build();
        List<Integer> memberIds = memberGroup.getStartedMemberIds();

        assertThat(memberIds.size(), is(numberOfMembers));

        for (int memberId : memberIds) {
            ClusterMember member = memberGroup.getClusterMember(memberId);
            assertThat(member.getActualContainingClassLoader() instanceof ChildFirstUrlClassLoader, is(true));
            assertThat(member.getActualContainingClassLoader(), not(member.getClass().getClassLoader()));
        }

        memberGroup.shutdownAll();
    }
}
