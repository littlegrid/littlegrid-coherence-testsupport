/*
 * Copyright (c) 2010-2020 Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the littlegrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid.impl;

import org.littlegrid.ReusableClusterMemberGroup;
import org.littlegrid.ReusableClusterMemberGroupRegistry;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Default implementation of reusable cluster member registry.
 */
public class DefaultReusableClusterMemberGroupRegistry implements ReusableClusterMemberGroupRegistry {
    private static final Logger LOGGER = Logger.getLogger(DefaultReusableClusterMemberGroupRegistry.class.getName());

    private static final ReusableClusterMemberGroupRegistry INSTANCE = new DefaultReusableClusterMemberGroupRegistry();

    /**
     * Default scope to facilitate testing.
     */
    final Map<Object, ReusableClusterMemberGroup> reusableClusterMemberGroupMap = new HashMap<>();

    public static ReusableClusterMemberGroupRegistry getInstance() {
        return INSTANCE;
    }

    @Override
    public ReusableClusterMemberGroup getClusterMemberGroup(final String identifier) {
        final ReusableClusterMemberGroup memberGroup = reusableClusterMemberGroupMap.get(identifier);

        LOGGER.info(format("Member group get using key: '%s' returned group: %s", identifier, memberGroup));

        return memberGroup;
    }

    @Override
    public void registerClusterMemberGroup(final String identifier,
                                           final ReusableClusterMemberGroup clusterMemberGroup) {

        LOGGER.info(format("Member group registered using key: '%s' with group of: %s", identifier, clusterMemberGroup));
        reusableClusterMemberGroupMap.put(identifier, clusterMemberGroup);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return format("Registry: %s", reusableClusterMemberGroupMap);
    }
}
