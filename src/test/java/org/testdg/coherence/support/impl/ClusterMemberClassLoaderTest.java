package org.testdg.coherence.support.impl;

import org.junit.Test;
import org.testdg.coherence.support.ClusterMember;
import org.testdg.coherence.support.ClusterMemberGroup;
import org.testdg.coherence.support.ClusterMemberGroupUtils;
import org.testdg.common.AbstractTest;

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
