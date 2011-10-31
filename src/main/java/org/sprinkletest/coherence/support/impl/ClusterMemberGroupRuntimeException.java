package org.sprinkletest.coherence.support.impl;

/**
 * Cluster member group runtime exception.
 */
@Deprecated
public class ClusterMemberGroupRuntimeException extends RuntimeException {
    /**
     * Constructor.
     *
     * @param message Message.
     */
    public ClusterMemberGroupRuntimeException(String message) {
        super(message);
    }

    /**
     * Constructor.
     *
     * @param cause Throwable.
     */
    public ClusterMemberGroupRuntimeException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructor.
     *
     * @param message Message.
     * @param cause   Throwable.
     */
    public ClusterMemberGroupRuntimeException(String message,
                                              Throwable cause) {

        super(message, cause);
    }
}
