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

package org.littlegrid.app;

import org.littlegrid.ClusterMemberGroupBuilder;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.BuildAndConfigureEnum.CONFIGURE_FOR_EXTEND_CLIENT;
import static org.littlegrid.ClusterMemberGroupBuilder.BUILDER_SYSTEM_PROPERTY_PREFIX_KEY;

/**
 * Extend client REPL application, providing access to CohQL and littlegrid features
 * for easy use and experimentation.
 *
 * @since 2.15
 */
public class ExtendClientReplApp {
    /**
     * Default scope to enable test coverage.
     */
    ExtendClientReplApp() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param args Arguments.
     */
    public static void main(final String[] args) {
        System.setProperty(BUILDER_SYSTEM_PROPERTY_PREFIX_KEY + "BuildAndConfigureForEnumName",
                CONFIGURE_FOR_EXTEND_CLIENT.name());

        final ClusterMemberGroupBuilder builder = ClusterMemberGroupUtils.newBuilder();

        if (builder.getClientCacheConfiguration().trim().length() == 0) {
            throw new IllegalStateException(
                    "No ClientCacheConfiguration file specified, cannot configure for Extend client - exiting");
        } else {
            final CommandDslShell shell = new CommandDslShell(System.in, System.out);
            shell.start(args);
        }
    }
}
