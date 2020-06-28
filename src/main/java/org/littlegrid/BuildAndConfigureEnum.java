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

package org.littlegrid;

/**
 * Build and configure enum, defining what type of 'client' or environment to configure
 * after the cluster member group has been built.
 *
 * @since 2.14
 * @since 3.0.0 - top-level enum with name changes.
 */
public enum BuildAndConfigureEnum {
    /**
     * Denotes that after the cluster member group has been built that the environment
     * should be configured for a storage-disabled client to interact with Coherence.
     */
    CONFIGURE_FOR_STORAGE_DISABLED_CLIENT,

    /**
     * Denotes that after the cluster member group has been built that the environment
     * should be configured for a Extend client to interact with Coherence.
     */
    CONFIGURE_FOR_EXTEND_CLIENT,

    /**
     * Denotes that after the cluster member group has been built then no special
     * configuration should be applied.
     */
    CONFIGURE_FOR_NO_CLIENT,

    /**
     * Denotes that after the cluster member group has been built that the environment
     * should be configured for a storage enabled member to interact with Coherence.
     */
    CONFIGURE_FOR_STORAGE_ENABLED_MEMBER
}
