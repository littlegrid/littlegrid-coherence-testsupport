/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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

import org.littlegrid.ClusterMemberGroupBuildException;
import org.littlegrid.IdentifiableException;
import org.littlegrid.support.ClassPathUtils;

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
import static org.littlegrid.ClusterMemberGroup.BuildExceptionReporter;

/**
 * Default exception reporter implementation that outputs useful information for
 * diagnosing the possible cause of a build exception - if the exception is a
 * form of @ClusterMemberGroupBuildException then additional information can also
 * be output.
 */
public class DefaultBuildExceptionReporter implements BuildExceptionReporter {
    private static final String SECTION_DIVIDER = "----------";
    private static final String KEY_VALUE_OUTPUT = "    %s = %s";


    /**
     * {@inheritDoc}
     */
    @Override
    public void report(final Throwable throwable,
                       final Map<String, String> builderKeysAndValues,
                       final Properties builderKeyToSystemPropertyNameMappings) {

        report(throwable, builderKeysAndValues, builderKeyToSystemPropertyNameMappings, null, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void report(final Throwable throwable,
                       final Map<String, String> builderKeysAndValues,
                       final Properties builderKeyToSystemPropertyNameMappings,
                       final String clusterMemberGroupInstanceClassName,
                       final String otherInformation) {

        final PrintStream out = System.out;

        out.print("http://littlegrid.bitbucket.org - ");

        if (throwable instanceof ClusterMemberGroupBuildException) {
            final ClusterMemberGroupBuildException buildException = (ClusterMemberGroupBuildException) throwable;

            out.println("Build exception");
            outputHeading(out);
            outputSuggestedExceptionReason(out, buildException);
            outputJavaHome(out);
            outputClassPath(out);
            outputClassPathInUse(out, buildException.getClassPathUrls());
            outputCurrentSystemProperties(out);
            outputSystemPropertiesBefore(out, buildException.getSystemPropertiesBeforeStartInvoked());
            outputSystemPropertiesApplied(out, buildException.getSystemPropertiesToBeApplied());
            outputSortedSystemPropertiesApplied(out, buildException.getSystemPropertiesToBeApplied());
            outputNumberOfMembers(out, buildException.getNumberOfMembers());
            outputClusterMemberInstanceClassName(out, buildException.getClusterMemberInstanceClassName());
            outputClusterMemberGroupInstanceClassName(out, clusterMemberGroupInstanceClassName);
            outputNumberOfThreadThreadsInStartUpPool(out, buildException.getNumberOfThreadsInStartUpPool());
            outputMemory(out);
            outputNetwork(out);
            outputSortedCurrentSystemProperties(out);
            outputBuilderKeysAndValues(out, builderKeysAndValues);
            outputBuilderKeyToSystemPropertyNameMapping(out, builderKeyToSystemPropertyNameMappings);
            outputException(out, throwable.getCause());
            outputOtherInformation(out, otherInformation);
            outputSuggestedExceptionReason(out, buildException);
        } else {
            out.println("Exception");
            outputHeading(out);
            outputJavaHome(out);
            outputClassPath(out);
            outputCurrentSystemProperties(out);
            outputMemory(out);
            outputNetwork(out);
            outputException(out, throwable);
            outputOtherInformation(out, otherInformation);
        }
    }

    private void outputSuggestedExceptionReason(final PrintStream out,
                                                final Throwable cause) {

        Throwable identifiableException = null;
        Throwable originalCause = cause;

        while (originalCause.getCause() != null) {
            originalCause = originalCause.getCause();

            if (originalCause instanceof IdentifiableException) {
                identifiableException = originalCause;

                break;
            }
        }

        if (identifiableException != null) {
            out.println(SECTION_DIVIDER);
            out.println("Possible exception reason: " + identifiableException);
        }
    }

    private void outputBuilderKeysAndValues(final PrintStream out,
                                            final Map<String, String> builderKeysAndValues) {

        out.println(SECTION_DIVIDER);
        out.println("Builder keys and values - sorted to help identification");

        final Map<String, String> map = new TreeMap<String, String>(builderKeysAndValues);

        for (final String key : map.keySet()) {
            String value = map.get(key);
            out.println(format(KEY_VALUE_OUTPUT, key, value));
        }
    }

    @SuppressWarnings("unchecked")
    private void outputBuilderKeyToSystemPropertyNameMapping(final PrintStream out,
                                                             final Properties builderKeyToSystemPropertyNameMapping) {

        out.println(SECTION_DIVIDER);
        out.println("Builder key to system property name mapping - sorted to help identification");

        final Map<String, String> map = new TreeMap(builderKeyToSystemPropertyNameMapping);

        for (final String key : map.keySet()) {
            String value = map.get(key);

            out.println(format(KEY_VALUE_OUTPUT, key, value));
        }
    }

    @SuppressWarnings("unchecked")
    private void outputSortedCurrentSystemProperties(final PrintStream out) {
        out.println(SECTION_DIVIDER);
        out.println("System properties current: all current system properties - sorted to help identification");

        final Map<String, String> map = new TreeMap(System.getProperties());

        for (final String key : map.keySet()) {
            String value = map.get(key);

            out.println(format(KEY_VALUE_OUTPUT, key, value));
        }
    }

    private void outputHeading(final PrintStream out) {
        out.println("**********************************************************************************************");
        out.println("Please check the FAQ (http://littlegrid.bitbucket.org/faq.html) for help on the exception");
        out.println("you've just had - also don't forget to check if a newer version of littlegrid is available.");
        out.println();
        out.println("If you would like help, then please email this entire exception report to: help@littlegrid.org");
        out.println();
        out.println("Exception occurred, trouble-shooting information below:");
        out.println("Name.....................: " + Info.getName());
        out.println("Version number...........: " + Info.getVersionNumber());
        out.println("Build date...............: " + Info.getBuildDate());
        out.println("Current time.............: " + new Date());
    }

    private void outputJavaHome(final PrintStream out) {
        out.println(SECTION_DIVIDER);
        out.println("Java home................: " + ClassPathUtils.getJavaHome(System.getProperties()));
    }

    private void outputClassPath(final PrintStream out) {
        out.println(SECTION_DIVIDER);
        out.println("Class path...............: " + ClassPathUtils.getClassPath(System.getProperties()));
    }

    private void outputClassPathInUse(final PrintStream out,
                                      final URL[] classPathUrls) {

        out.println(SECTION_DIVIDER);
        out.println("Class path in use........: " + Arrays.toString(classPathUrls));
    }

    private void outputSystemPropertiesBefore(final PrintStream out,
                                              final Properties systemPropertiesBeforeStartInvoked) {

        out.println(SECTION_DIVIDER);
        out.println("System properties before.: " + systemPropertiesBeforeStartInvoked);
    }

    private void outputSystemPropertiesApplied(final PrintStream out,
                                               final Properties systemPropertiesToBeApplied) {

        out.println(SECTION_DIVIDER);
        out.println("System properties applied: " + systemPropertiesToBeApplied);
    }

    private void outputMemory(final PrintStream out) {
        final int oneMB = 1024 * 1024;

        out.println(SECTION_DIVIDER);
        out.println(format("Memory...................: max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private void outputNetwork(final PrintStream out) {
        out.println(SECTION_DIVIDER);

        try {
            final InetAddress inetAddress = InetAddress.getLocalHost();

            out.println(format("Network..................: hostname: %s, address: %s",
                    inetAddress.getHostName(), inetAddress.getHostAddress()));
        } catch (UnknownHostException e) {
            out.println("Unable to determine hostname/IP address");
        }
    }

    @SuppressWarnings("unchecked")
    private void outputSortedSystemPropertiesApplied(final PrintStream out,
                                                     final Properties systemPropertiesToBeApplied) {

        out.println(SECTION_DIVIDER);
        out.println("System properties applied: sorted to help identification");

        final Map<String, String> map = new TreeMap(systemPropertiesToBeApplied);

        for (final String key : map.keySet()) {
            String value = map.get(key);

            out.println(format(KEY_VALUE_OUTPUT, key, value));
        }
    }

    private void outputCurrentSystemProperties(final PrintStream out) {
        out.println(SECTION_DIVIDER);
        out.println("Current system properties: " + System.getProperties());
    }

    private void outputNumberOfThreadThreadsInStartUpPool(final PrintStream out,
                                                          final int numberOfThreadsInStartUpPool) {

        out.println(SECTION_DIVIDER);
        out.println("Number of thread in pool.: " + numberOfThreadsInStartUpPool);
    }

    private void outputClusterMemberInstanceClassName(final PrintStream out,
                                                      final String clusterMemberInstanceClassName) {

        out.println(SECTION_DIVIDER);
        out.println("Cluster member class name: " + clusterMemberInstanceClassName);
    }

    private void outputClusterMemberGroupInstanceClassName(final PrintStream out,
                                                           final String clusterMemberGroupInstanceClassName) {

        out.println(SECTION_DIVIDER);
        out.println("Cluster member group name: " + clusterMemberGroupInstanceClassName);
    }

    private void outputNumberOfMembers(final PrintStream out,
                                       final int numberOfMembers) {

        out.println(SECTION_DIVIDER);
        out.println("Number of members........: " + numberOfMembers);
    }

    private void outputException(final PrintStream out,
                                 final Throwable cause) {

        out.println(SECTION_DIVIDER);
        out.println("Full exception...........: detailed below");
        cause.printStackTrace(out);
    }

    private void outputOtherInformation(final PrintStream out,
                                        final String otherInformation) {

        if (otherInformation != null) {
            out.println(SECTION_DIVIDER);
            out.println("Other information........: detailed below");
            out.println(otherInformation);
        }
    }
}
