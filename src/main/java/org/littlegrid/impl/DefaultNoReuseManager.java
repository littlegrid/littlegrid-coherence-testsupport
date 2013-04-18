package org.littlegrid.impl;

import org.littlegrid.ClusterMemberGroup;

import static org.littlegrid.ClusterMemberGroup.ReuseManager;

/**
 * Default no re-use strategy handler.
 */
public class DefaultNoReuseManager implements ReuseManager {
    @Override
    public void registerInstanceUse(final Object builderKeyUsedToConstructMemberGroup,
                                    final ClusterMemberGroup constructedMemberGroup) {
        // Do nothing
    }

    @Override
    public ClusterMemberGroup getRegisteredInstance(final Object builderKey) {
        return null;
    }

    @Override
    public boolean isFinalShutdownAllAdvised() {
        return true;
    }
}
