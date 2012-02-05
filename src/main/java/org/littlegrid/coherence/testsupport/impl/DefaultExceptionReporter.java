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

package org.littlegrid.coherence.testsupport.impl;

import org.littlegrid.coherence.testsupport.ClusterMemberGroup;
import org.littlegrid.coherence.testsupport.ClusterMemberGroupStartUpException;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;

/**
 * Default exception reporter implementation
 */
public class DefaultExceptionReporter implements ClusterMemberGroup.ExceptionReporter {
    /**
     * {@inheritDoc}
     */
    @Override
    public void report(final Throwable throwable) {
        final PrintStream out = System.out;
        
        if (throwable instanceof ClusterMemberGroupStartUpException) {
            final ClusterMemberGroupStartUpException startUpException = (ClusterMemberGroupStartUpException) throwable;

            System.out.println("Oh dear - " + Arrays.toString(startUpException.getClassPathUrls()) +
                    startUpException.getSystemPropertiesBeforeStartInvoked());

        } else {
            outputHeading(out);
            outputJavaHome(out);
            outputClassPath(out);
        }
    }
    
    private void outputHeading(final PrintStream out) {
        out.println("Exception occurred, details to help trouble-shooting below");
        out.println("==========================================================");
    }
    
    private void outputJavaHome(final PrintStream out)  {
        out.println("Java home.: " + System.getProperty("java.home"));
    }
    
    private void outputClassPath(final PrintStream out) {
        out.println("Class path: " + System.getProperty("java.class.path"));
    }
}
