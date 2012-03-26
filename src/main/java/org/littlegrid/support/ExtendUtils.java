/*
 * Copyright (c) 2010-2012 Jonathan Hall.
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
 * Neither the name of the LittleGrid nor the names of its contributors may
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

package org.littlegrid.support;

import com.tangosol.io.pof.PofReader;
import com.tangosol.io.pof.PofWriter;
import com.tangosol.io.pof.PortableObject;
import com.tangosol.net.AbstractInvocable;
import com.tangosol.net.InvocationService;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Extend test support, containing useful classes and methods to aid testing involving
 * Extend proxies.
 *
 * @since 2.6
 */
public final class ExtendUtils {
    /**
     * Private constructor to prevent creation.
     */
    private ExtendUtils() {
    }

    public static int getClusterSizeThatExtendClientIsConnectedTo(final InvocationService invocationService) {
        final Map result = invocationService.query(new ExtendUtils.GetClusterSizeInvocable(), null);

        final List<Integer> list = new ArrayList<Integer>(result.values());

        return list.get(0);
    }


    public static int getExtendProxyMemberIdThatClientIsConnectedTo(final InvocationService invocationService) {
        final Map result = invocationService.query(new ExtendUtils.GetExtendProxyMemberIdInvocable(), null);

        final List<Integer> list = new ArrayList<Integer>(result.values());

        return list.get(0);
    }

    /**
     * Simple invocable to return the cluster size - useful for Extend-based clients
     * to check cluster size is as expected.
     */
    public static final class GetClusterSizeInvocable extends AbstractInvocable
            implements PortableObject {

        @Override
        public void run() {
            setResult(getService().getCluster().getMemberSet().size());
        }

        @Override
        public void readExternal(final PofReader reader)
                throws IOException {
        }

        @Override
        public void writeExternal(final PofWriter writer)
                throws IOException {
        }
    }

    /**
     * Simple invocable to return the Extend proxy member Id that the Extend client is
     * connected to - this is useful for instance in tests were that particular proxy
     * server is required to be stopped to test failover.
     */
    public static final class GetExtendProxyMemberIdInvocable extends AbstractInvocable
            implements PortableObject {

        @Override
        public void run() {
            setResult(getService().getCluster().getLocalMember().getId());
        }

        @Override
        public void readExternal(final PofReader reader)
                throws IOException {
        }

        @Override
        public void writeExternal(final PofWriter writer)
                throws IOException {
        }
    }
}
