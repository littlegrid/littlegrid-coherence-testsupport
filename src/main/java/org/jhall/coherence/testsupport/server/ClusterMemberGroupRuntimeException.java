package org.jhall.coherence.testsupport.server;

import org.jhall.coherence.testsupport.common.AbstractTestSupportRuntimeException;

/**
 * Cluster member group runtime exception.
 */
public class ClusterMemberGroupRuntimeException extends AbstractTestSupportRuntimeException {
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
