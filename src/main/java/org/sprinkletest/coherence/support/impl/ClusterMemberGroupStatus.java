package org.sprinkletest.coherence.support.impl;

/**
 * Enumerated type representing cluster member group life-cycle status.
 */
public enum ClusterMemberGroupStatus {
    NEVER_STARTED,
    INITIALIZATION_PROBLEM,
    STARTING, START_FAILED, RUNNING,
    HALTING,
    SHUTDOWN_FAILED,
    STOP_FAILED,
    HALTED
}


