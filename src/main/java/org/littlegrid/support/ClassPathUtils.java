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

package org.littlegrid.support;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Class path utilities class.
 */
public final class ClassPathUtils {
    private static final Logger LOGGER = Logger.getLogger(ClassPathUtils.class.getName());
    private static final String PATH_SEPARATOR = "path.separator";
    private static final String JAVA_CLASS_PATH = "java.class.path";
    private static final String SUREFIRE_TEST_CLASS_PATH = "surefire.test.class.path";
    private static final String JAVA_HOME = "java.home";

    /**
     * Private constructor to prevent creation.
     */
    private ClassPathUtils() {
    }

    /**
     * Convenience method to get path separator.
     *
     * @param systemProperties System properties.
     * @return path separator.
     */
    public static String getPathSeparator(final Properties systemProperties) {
        return systemProperties.getProperty(PATH_SEPARATOR);
    }

    /**
     * Returns the current class path - understands if class path is being controlled through
     * Surefire (Maven).
     *
     * @param systemProperties System properties.
     * @return class path.
     */
    public static String getClassPath(final Properties systemProperties) {
        if (systemProperties.containsKey(SUREFIRE_TEST_CLASS_PATH)) {
            LOGGER.fine("Note: class path is being controlled by Surefire");

            return systemProperties.getProperty(SUREFIRE_TEST_CLASS_PATH);
        } else {
            return systemProperties.getProperty(JAVA_CLASS_PATH);
        }
    }

    /**
     * Convenience method to get Java home.
     *
     * @param systemProperties System properties.
     * @return java home.
     */
    public static String getJavaHome(final Properties systemProperties) {
        return systemProperties.getProperty(JAVA_HOME);
    }

    /**
     * Returns the class path as URLs excluding any JARs that have been specified to be excluded.
     *
     * @param javaHomePath               Java home.
     * @param classPath                  Class path.
     * @param pathSeparator              Path separator.
     * @param jarsToExcludeFromClassPath JARs to exclude from final class path.
     * @return class path.
     */
    public static URL[] getClassPathUrlsExcludingJavaHome(final String javaHomePath,
                                                          final String classPath,
                                                          final String pathSeparator,
                                                          final String jarsToExcludeFromClassPath) {

        final String[] classPathArray = classPath.split(pathSeparator);

        final List<URL> classPathUrls = new ArrayList<URL>();

        for (final String partOfClassPath : classPathArray) {
            if (!partOfClassPath.startsWith(javaHomePath)) {
                boolean includeInClassPath = true;

                if (jarsToExcludeFromClassPath != null) {
                    for (final String jarToExclude : jarsToExcludeFromClassPath.split(",")) {
                        final String trimmedJarToExclude = jarToExclude.trim();

                        if (trimmedJarToExclude.length() != 0 && partOfClassPath.contains(trimmedJarToExclude)) {
                            LOGGER.fine(format("JAR: '%s' specified for exclusion from class path",
                                    trimmedJarToExclude));

                            includeInClassPath = false;
                        }
                    }
                }

                if (includeInClassPath) {
                    try {
                        classPathUrls.add(new File(partOfClassPath).toURI().toURL());
                    } catch (MalformedURLException e) {
                        throw new IllegalStateException(
                                format("Part of class path '%s' has a malformed URL", partOfClassPath));
                    }
                }
            }
        }

        return classPathUrls.toArray(new URL[classPathUrls.size()]);
    }
}
