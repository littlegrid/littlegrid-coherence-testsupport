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

import java.util.Map;
import java.util.Properties;

/**
 * Build exception reporter, reports useful exception information in a form to help with
 * trouble-shooting.
 *
 * @since 3.0.0 - top-level interface.
 */
public interface BuildExceptionReporter {
    /**
     * Report on the exception.
     *
     * @param throwable                              Throwable.
     * @param builderKeysAndValues                   Builder keys and values.
     * @param builderKeyToSystemPropertyNameMappings Builder key to system property name mapping.
     */
    void report(Throwable throwable,
                Map<String, String> builderKeysAndValues,
                Properties builderKeyToSystemPropertyNameMappings);

    /**
     * Report on the exception.
     *
     * @param throwable                              Throwable.
     * @param builderKeysAndValues                   Builder keys and values.
     * @param builderKeyToSystemPropertyNameMappings Builder key to system property name mapping.
     * @param clusterMemberGroupInstanceClassName    Cluster member group instance class name.
     * @param otherInformation                       Other information that may be builder specific and useful
     *                                               to help identify the problem.
     * @since 2.15
     */
    void report(Throwable throwable,
                Map<String, String> builderKeysAndValues,
                Properties builderKeyToSystemPropertyNameMappings,
                String clusterMemberGroupInstanceClassName,
                String otherInformation);
}
