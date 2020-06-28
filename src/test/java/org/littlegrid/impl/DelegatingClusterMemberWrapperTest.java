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
import org.littlegrid.IdentifiableException;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.littlegrid.IdentifiableException.ReasonEnum.JOIN_TIMEOUT_MILLISECONDS_TOO_SMALL;
import static org.littlegrid.IdentifiableException.ReasonEnum.SUSPECTED_AUTOSTART_EXCEPTION;
import static org.littlegrid.impl.DelegatingClusterMemberWrapper.ERROR_INSTANTIATING_FILTER_WITH_NAME_GZIP;
import static org.littlegrid.impl.DelegatingClusterMemberWrapper.VALUE_OUT_OF_RANGE_1000;

/**
 * Delegating cluster member wrapper tests.
 */
public final class DelegatingClusterMemberWrapperTest {
    @Test(expected = IllegalStateException.class)
    public void invoke() {
        DelegatingClusterMemberWrapper.invokeMethod("a-string", "nonExistentMethod");
    }

    @Test
    public void identifiedExceptionWhenAutostartProblem() {
        final IdentifiableException exception = (IdentifiableException)
                DelegatingClusterMemberWrapper.exceptionAfterAttemptedIdentification(
                        new RuntimeException(new RuntimeException(ERROR_INSTANTIATING_FILTER_WITH_NAME_GZIP)));

        assertThat(exception.getReasonEnum(), is(SUSPECTED_AUTOSTART_EXCEPTION));
    }

    @Test
    public void identifiedExceptionWhenJoinTimeoutTooSmall() {
        final IdentifiableException exception = (IdentifiableException)
                DelegatingClusterMemberWrapper.exceptionAfterAttemptedIdentification(
                        new RuntimeException(new RuntimeException(VALUE_OUT_OF_RANGE_1000)));

        assertThat(exception.getReasonEnum(), is(JOIN_TIMEOUT_MILLISECONDS_TOO_SMALL));
    }
}
