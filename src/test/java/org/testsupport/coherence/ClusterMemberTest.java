package org.testsupport.coherence;

import org.junit.Test;
import org.testsupport.common.AbstractTest;
import org.testsupport.common.net.ChildFirstUrlClassLoader;

import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;
import static org.testsupport.coherence.ClusterMemberGroupUtils.newClusterMemberGroupBuilder;

/**
 * Cluster member tests.
 */
public class ClusterMemberTest extends AbstractTest {
    @Test
    public void getClassLoader() {
        final int numberOfMembers = 3;
        ClusterMemberGroup memberGroup = newClusterMemberGroupBuilder().setNumberOfMembers(numberOfMembers).build().startAll();
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
