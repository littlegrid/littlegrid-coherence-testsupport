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

package org.littlegrid.support;

import org.junit.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Class path utilities tests.
 */
public class ClassPathUtilsTest {
    private static final String PATH_SEPARATOR_KEY = "path.separator";
    private static final String PATH_SEPARATOR_VALUE = ":";

    private static final String JAVA_HOME_KEY = "java.home";
    private static final String JAVA_HOME_VALUE = "/path/java/1.6/jre";

    private static final String COHERENCE_JAR = "/path/coherence.jar";
    private static final String OTHER_JAR = "/path/other.jar";

    private static final String JAVA_CLASS_PATH_KEY = "java.class.path";
    private static final String JAVA_CLASS_PATH_VALUE = JAVA_HOME_VALUE + "/lib/deploy.jar" + PATH_SEPARATOR_VALUE
            + JAVA_HOME_VALUE + "/rt.jar" + PATH_SEPARATOR_VALUE
            + COHERENCE_JAR + PATH_SEPARATOR_VALUE + OTHER_JAR;

    private static final String SUREFIRE_TEST_CLASS_PATH_KEY = "surefire.test.class.path";
    private static final String SUREFIRE_TEST_CLASS_PATH_VALUE = JAVA_CLASS_PATH_VALUE;


    @Test(expected = UnsupportedOperationException.class)
    public void construct() {
        new ClassPathUtils();
    }

    @Test
    public void pathSeparator() {
        assertThat(ClassPathUtils.getPathSeparator(getPopulatedProperties()), is(PATH_SEPARATOR_VALUE));
    }

    @Test
    public void standardClassPath() {
        assertThat(ClassPathUtils.getClassPath(getPopulatedProperties()), is(JAVA_CLASS_PATH_VALUE));
    }

    @Test
    public void javaHome() {
        assertThat(ClassPathUtils.getJavaHome(getPopulatedProperties()), notNullValue());
    }

    @Test
    public void surefireClassPath() {
        final Properties systemProperties = getPopulatedProperties();
        systemProperties.setProperty(JAVA_CLASS_PATH_KEY, "surefire.jar");
        systemProperties.setProperty(SUREFIRE_TEST_CLASS_PATH_KEY, SUREFIRE_TEST_CLASS_PATH_VALUE);

        assertThat(ClassPathUtils.getClassPath(systemProperties), is(SUREFIRE_TEST_CLASS_PATH_VALUE));
    }

    @Test
    public void getClassPathUrlsExcludingJavaHomeNoExcludes()
            throws MalformedURLException {

        final int expectedUrlCount = 2;

        final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                JAVA_HOME_VALUE, JAVA_CLASS_PATH_VALUE, PATH_SEPARATOR_VALUE, null);

        assertThat(classPathUrls.length, is(expectedUrlCount));

        int urlCount = 0;

        for (final URL classPathUrl : classPathUrls) {
            final String classPath = classPathUrl.toString();

            if (classPath.contains(COHERENCE_JAR)) {
                urlCount++;
            }

            if (classPath.contains(OTHER_JAR)) {
                urlCount++;
            }
        }

        assertThat(urlCount, is(expectedUrlCount));
    }

    @Test
    public void getClassPathUrlsExcludingJavaHomeWithExcludes()
            throws MalformedURLException {

        final int expectedUrlCount = 1;
        final String jarsToExcludeFromClassPath = ",, , coherence ,, , ";

        final URL[] classPathUrls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                JAVA_HOME_VALUE, JAVA_CLASS_PATH_VALUE, PATH_SEPARATOR_VALUE, jarsToExcludeFromClassPath);

        assertThat(classPathUrls.length, is(expectedUrlCount));
    }

    private Properties getPopulatedProperties() {
        final Properties properties = new Properties();
        properties.setProperty(PATH_SEPARATOR_KEY, PATH_SEPARATOR_VALUE);
        properties.setProperty(JAVA_HOME_KEY, JAVA_HOME_VALUE);
        properties.setProperty(JAVA_CLASS_PATH_KEY, JAVA_CLASS_PATH_VALUE);

        return properties;
    }
}
