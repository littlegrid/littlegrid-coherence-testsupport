package org.jhall.coherence.testsupport.server.impl;

import org.junit.Test;

import static org.jhall.coherence.testsupport.server.impl.AbstractDefaultLocalProcessClusterMemberGroupImplTest.StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN;

/**
 * Default local process cluster member group larger tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplLargerTest extends
        AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndStopThenShutdownLargeMemberGroup() {
        final int numberOfThreads = LARGE_TEST_CLUSTER_SIZE;

        performStartOptionallyStopAndThenShutdownAndCheck(PERFORM_STOP_THEN_SHUTDOWN,
                LARGE_TEST_CLUSTER_SIZE, numberOfThreads);
    }
}
