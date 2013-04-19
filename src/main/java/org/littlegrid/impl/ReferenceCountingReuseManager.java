package org.littlegrid.impl;

import org.littlegrid.ClusterMemberGroup;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.impl.DefaultClusterMemberGroup.ReuseManager;

/**
 * Reference counting re-use strategy handler
 */
public class ReferenceCountingReuseManager implements ReuseManager {
    private static final Logger LOGGER = Logger.getLogger(ReferenceCountingReuseManager.class.getName());

    private Object builderKey;
    private static final Map<Object, Integer> BUILDER_TO_COUNT_REGISTRY = new HashMap<Object, Integer>();
    private static final Map<Object, ClusterMemberGroup> BUILDER_TO_MEMBER_GROUP_REGISTRY =
            new HashMap<Object, ClusterMemberGroup>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void registerInstanceUse(final Object builderKeyUsedToConstructMemberGroup,
                                    final ClusterMemberGroup constructedMemberGroup) {

        builderKey = builderKeyUsedToConstructMemberGroup;

        Integer count = BUILDER_TO_COUNT_REGISTRY.get(builderKey);

        if (count == null) {
            count = 1;
        } else {
            count++;
        }

        LOGGER.info(format("Reference count %d for key '%s'", count, builderKey));
        BUILDER_TO_COUNT_REGISTRY.put(builderKey, count);
        BUILDER_TO_MEMBER_GROUP_REGISTRY.put(builderKey, constructedMemberGroup);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup getRegisteredInstance(final Object builderKey) {
        return BUILDER_TO_MEMBER_GROUP_REGISTRY.get(builderKey);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isFinalShutdownAllAdvised() {
        Integer count = BUILDER_TO_COUNT_REGISTRY.get(builderKey);

        if (count == null) {
            throw new UnsupportedOperationException();
        } else {
            count--;

            LOGGER.info(format("Reference count %d for key '%s'", count, builderKey));

            if (count == 0) {
                BUILDER_TO_COUNT_REGISTRY.remove(builderKey);
                BUILDER_TO_MEMBER_GROUP_REGISTRY.remove(builderKey);

                return true;
            } else {
                BUILDER_TO_COUNT_REGISTRY.put(builderKey, count);

                return false;
            }
        }
    }
}
