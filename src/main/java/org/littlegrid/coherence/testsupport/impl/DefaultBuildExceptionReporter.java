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
import org.littlegrid.coherence.testsupport.ClusterMemberGroupBuildException;

import java.io.PrintStream;
import java.net.InetAddress;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import static java.lang.String.format;

/**
 * Default exception reporter implementation that outputs useful information for
 * diagnosing the possible cause of a build exception - if the exception is a
 * form of @ClusterMemberGroupBuildException then additional information can also
 * be output.
 */
public class DefaultBuildExceptionReporter implements ClusterMemberGroup.BuildExceptionReporter {
    /**
     * {@inheritDoc}
     */
    @Override
    public void report(final Throwable throwable) {
        final PrintStream out = System.out;

        if (throwable instanceof ClusterMemberGroupBuildException) {
            final ClusterMemberGroupBuildException buildException = (ClusterMemberGroupBuildException) throwable;

            outputHeading(out);
            outputJavaHome(out);
            outputClassPath(out);
            outputClassPathInUse(out, buildException.getClassPathUrls());
            outputCurrentSystemProperties(out);
            outputSystemPropertiesBefore(out, buildException.getSystemPropertiesBeforeStartInvoked());
            outputSystemPropertiesApplied(out, buildException.getSystemPropertiesToBeApplied());
            outputNumberOfMembers(out, buildException.getNumberOfMembers());
            outputClusterMemberInstanceClassName(out, buildException.getClusterMemberInstanceClassName());
            outputNumberOfThreadThreadsInStartUpPool(out, buildException.getNumberOfThreadsInStartUpPool());
            outputMemory(out);
            outputNetwork(out);
            outputSortedSystemPropertiesApplied(out, buildException.getSystemPropertiesToBeApplied());
            outputSortedCurrentSystemProperties(out);
            outputException(out, throwable.getCause());
        } else {
            outputHeading(out);
            outputJavaHome(out);
            outputClassPath(out);
            outputCurrentSystemProperties(out);
            outputMemory(out);
            outputNetwork(out);
            outputException(out, throwable);
        }
    }

    private void outputSortedCurrentSystemProperties(final PrintStream out) {
        out.println("System properties current: all current system properties - sorted to help identification");

        Map<String, String> map = new TreeMap(System.getProperties());

        for (final String key : map.keySet()) {
            String value = map.get(key);

            out.println(format("    key=%s, value=%s", key, value));
        }
    }

    private void outputHeading(final PrintStream out) {
        out.println("******************************************************");
        out.println("Exception occurred, trouble-shooting information below");
        out.println("Current time.............: " + new Date());
    }

    private void outputJavaHome(final PrintStream out) {
        out.println("Java home................: " + System.getProperty("java.home"));
    }

    private void outputClassPath(final PrintStream out) {
        out.println("Class path...............: " + System.getProperty("java.class.path"));
    }

    private void outputClassPathInUse(final PrintStream out,
                                      final URL[] classPathUrls) {

        out.println("Class path in use........: " + Arrays.toString(classPathUrls));
    }

    private void outputSystemPropertiesBefore(final PrintStream out,
                                              final Properties systemPropertiesBeforeStartInvoked) {

        out.println("System properties before.: " + systemPropertiesBeforeStartInvoked);
    }

    private void outputSystemPropertiesApplied(final PrintStream out,
                                               final Properties systemPropertiesToBeApplied) {

        out.println("System properties applied: " + systemPropertiesToBeApplied);
    }

    private void outputMemory(final PrintStream out) {
        final int oneMB = 1024 * 1024;

        out.println(format("Memory...................: max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private void outputNetwork(final PrintStream out) {
        try {
            final InetAddress inetAddress = InetAddress.getLocalHost();

            out.println(format("Network..................: hostname: %s, address: %s",
                    inetAddress.getHostName(), inetAddress.getHostAddress()));
        } catch (UnknownHostException e) {
            out.println("Unable to determine hostname/IP address");
        }
    }

    private void outputSortedSystemPropertiesApplied(final PrintStream out,
                                                     final Properties systemPropertiesToBeApplied) {

        out.println("System properties applied: sorted to help identification");

        Map<String, String> map = new TreeMap(systemPropertiesToBeApplied);

        for (final String key : map.keySet()) {
            String value = map.get(key);

            out.println(format("    key=%s, value=%s", key, value));
        }
    }

    private void outputCurrentSystemProperties(final PrintStream out) {
        out.println("Current system properties: " + System.getProperties());
    }

    private void outputNumberOfThreadThreadsInStartUpPool(final PrintStream out,
                                                          final int numberOfThreadsInStartUpPool) {

        out.println("Number of thread in pool.: " + numberOfThreadsInStartUpPool);
    }

    private void outputClusterMemberInstanceClassName(final PrintStream out,
                                                      final String clusterMemberInstanceClassName) {

        out.println("Cluster member class name: " + clusterMemberInstanceClassName);
    }

    private void outputNumberOfMembers(final PrintStream out,
                                       final int numberOfMembers) {

        out.println("Number of members........: " + numberOfMembers);
    }

    private void outputException(final PrintStream out,
                                 final Throwable cause) {

        out.println("Full exception...........: detailed below");
        cause.printStackTrace(out);
    }
}
