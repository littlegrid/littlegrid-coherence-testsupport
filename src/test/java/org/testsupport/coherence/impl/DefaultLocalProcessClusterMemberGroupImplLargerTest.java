package org.testsupport.coherence.impl;

import org.junit.Test;

/**
 * Default local process cluster member group larger tests.
 */
public class DefaultLocalProcessClusterMemberGroupImplLargerTest extends
        AbstractDefaultLocalProcessClusterMemberGroupImplTest {

    @Test
    public void startAndStopThenShutdownLargeMemberGroup() {
        final int numberOfThreads = LARGE_TEST_CLUSTER_SIZE;

        performStartOptionallyStopAndThenShutdownAndCheck(StopShutdownMechanism.PERFORM_STOP_THEN_SHUTDOWN,
                LARGE_TEST_CLUSTER_SIZE, numberOfThreads);
    }
}
