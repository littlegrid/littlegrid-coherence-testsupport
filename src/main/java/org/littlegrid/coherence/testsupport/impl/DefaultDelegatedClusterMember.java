package org.littlegrid.coherence.testsupport.impl;

/**
 * Default cluster member delegate implementation - this doesn't perform any before
 * or after actions.
 */
public final class DefaultDelegatedClusterMember extends AbstractDelegatedClusterMember {

    /**
     * {@inheritDoc}
     */
    @Override
    public void doBeforeStart() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void doAfterStart() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void doBeforeShutdown() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void doAfterShutdown() {
    }
}
