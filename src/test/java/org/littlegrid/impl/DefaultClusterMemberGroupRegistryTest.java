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

import org.junit.Test;
import org.littlegrid.ClusterMemberGroupBuilder;
import org.littlegrid.ClusterMemberGroupUtils;
import org.littlegrid.ReusableClusterMemberGroup;
import org.littlegrid.ReusableClusterMemberGroupRegistry;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Default cluster member group builder tests.
 */
public final class DefaultClusterMemberGroupRegistryTest {
    @Test
    public void registryGetWhenEntryDoesNotExist() {
        final ReusableClusterMemberGroupRegistry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        assertThat(getRegistrySize(), is(0));
        assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), nullValue());
    }

    @Test
    public void registryGetWhenEntryDoesExist() {
        final ReusableClusterMemberGroupRegistry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        registry.registerClusterMemberGroup(getIdentifier(builder), getClusterMemberGroup());

        assertThat(getRegistrySize(), is(1));
        assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesNotExist() {
        final ReusableClusterMemberGroupRegistry registry = getRegistryAndClearContents();

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        registry.registerClusterMemberGroup(getIdentifier(builder), getClusterMemberGroup());
        assertThat(getRegistrySize(), is(1));
        assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), notNullValue());
    }

    @Test
    public void registryRegisterWhenEntryDoesExist() {
        final ReusableClusterMemberGroupRegistry registry = getRegistryAndClearContents();

        {
            final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

            final ReusableClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(getIdentifier(builder), memberGroup);

            assertThat(getRegistrySize(), is(1));
            assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), notNullValue());
            assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), is(memberGroup));
        }

        {
            final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

            final ReusableClusterMemberGroup memberGroup = getClusterMemberGroup();

            registry.registerClusterMemberGroup(getIdentifier(builder), memberGroup);

            assertThat(getRegistrySize(), is(1));
            assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), notNullValue());
            assertThat(registry.getClusterMemberGroup(getIdentifier(builder)), is(memberGroup));
        }
    }

    private String getIdentifier(ClusterMemberGroupBuilder builder) {
        return Integer.toString(builder.hashCode());
    }

    private ReusableClusterMemberGroup getClusterMemberGroup() {
        return new UsageCountingClusterMemberGroup(new DefaultCallbackHandler(), 0, 0, 0, 0, 0);
    }

    private ReusableClusterMemberGroupRegistry getRegistryAndClearContents() {
        final ReusableClusterMemberGroupRegistry registry = DefaultReusableClusterMemberGroupRegistry.getInstance();

        ((DefaultReusableClusterMemberGroupRegistry) registry).reusableClusterMemberGroupMap.clear();

        return registry;
    }

    private int getRegistrySize() {
        final ReusableClusterMemberGroupRegistry registry = DefaultReusableClusterMemberGroupRegistry.getInstance();

        return ((DefaultReusableClusterMemberGroupRegistry) registry).reusableClusterMemberGroupMap.size();
    }
}
