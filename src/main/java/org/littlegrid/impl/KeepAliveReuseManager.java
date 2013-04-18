package org.littlegrid.impl;

/**
 * Default no re-use strategy handler.
 */
public class KeepAliveReuseManager extends ReferenceCountingReuseManager {
    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isFinalShutdownAllAdvised() {
        return false;
    }
}
