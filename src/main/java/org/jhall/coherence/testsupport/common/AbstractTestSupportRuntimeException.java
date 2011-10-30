package org.jhall.coherence.testsupport.common;

/**
 * Abstract test support runtime exception.
 */
public abstract class AbstractTestSupportRuntimeException extends RuntimeException {
    /**
     * Constructor.
     *
     * @param message Message.
     */
    public AbstractTestSupportRuntimeException(String message) {
        super(message);
    }

    /**
     * Constructor.
     *
     * @param cause Throwable.
     */
    protected AbstractTestSupportRuntimeException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructor.
     *
     * @param message Message.
     * @param cause   Throwable.
     */
    public AbstractTestSupportRuntimeException(String message,
                                               Throwable cause) {

        super(message, cause);
    }
}
