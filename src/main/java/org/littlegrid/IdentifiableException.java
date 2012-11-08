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

import static java.lang.String.format;

/**
 * Identifiable exception.
 *
 * @since 2.13
 */
public class IdentifiableException extends RuntimeException {
    private final ReasonEnum reasonEnum;

    /**
     * Types of exception reason.
     */
    public static enum ReasonEnum {
        /** Exception reason. */
        SECURITY_EXCEPTION,

        /** Exception reason. */
        SUSPECTED_AUTOSTART_EXCEPTION,

        /** Exception reason. */
        UNABLE_TO_SET_BEAN_PROPERTY,

        /** Exception reason. */
        CHECK_CHILD_FIRST_CLASS_PATH_IN_USE,

        /** Exception reason. */
        CHECK_CACHE_CONFIGURATION_FILE_BEING_USED
    }

    /**
     * Constructor.
     *
     * @param message    Message.
     * @param cause      Cause of exception.
     * @param reasonEnum Exception enum if exception is potentially recognisable.
     */
    public IdentifiableException(final String message,
                                 final Throwable cause,
                                 final ReasonEnum reasonEnum) {
        super(message, cause);
        this.reasonEnum = reasonEnum;
    }

    /**
     * Constructor.
     *
     * @param reasonEnum Exception enum if exception is potentially recognisable.
     * @param message    Message.
     */
    public IdentifiableException(final String message,
                                 final ReasonEnum reasonEnum) {
        super(message);
        this.reasonEnum = reasonEnum;
    }

    /**
     * Returns the suggested exception reason.
     *
     * @return suggested reason.
     */
    public ReasonEnum getReasonEnum() {
        return reasonEnum;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return format(format("%s %s", reasonEnum.toString(), getMessage()));
    }
}
