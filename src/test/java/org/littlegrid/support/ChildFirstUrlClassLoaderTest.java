/*
 * Copyright (c) 2010-2013 Jonathan Hall.
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

import org.junit.Ignore;
import org.junit.Test;
import org.littlegrid.IdentifiableException;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.littlegrid.IdentifiableException.ReasonEnum.SECURITY_EXCEPTION;

/**
 * Child first URL class loader tests.
 */
public final class ChildFirstUrlClassLoaderTest {
    private static final String TARGET_TEST_CLASSES_DIRECTORY = "./target/test-classes";

    @Test
    public void loadClassFromChildClassLoader()
            throws Throwable {

        final URL url = new File(TARGET_TEST_CLASSES_DIRECTORY).toURI().toURL();
        final ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(new URL[]{url},
                this.getClass().getClassLoader());

        final Class clazz = childFirstLoader.loadClass(Dummy.class.getName());

        final Object objectFromClassLoadedByChildFirst = clazz.newInstance();
        final Dummy dummy = new Dummy();

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), is(childFirstLoader));
        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(),
                not(dummy.getClass().getClassLoader()));
    }

    @Test
    @Ignore
    public void delegateWhenLoadingCoreClass()
            throws Throwable {

        final Properties systemProperties = System.getProperties();

        final URL[] urls = ClassPathUtils.getClassPathUrlsExcludingJavaHome(
                "made-up-java-home-so-real-one-will-not-be-excluded",
                ClassPathUtils.getClassPath(systemProperties),
                ClassPathUtils.getPathSeparator(systemProperties),
                null);

        final ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(urls,
                this.getClass().getClassLoader());

        try {
            childFirstLoader.loadClass(String.class.getName());

            fail("An identifiable exception was expected");
        } catch (IdentifiableException e) {
            assertThat(e.getReasonEnum(), is(SECURITY_EXCEPTION));
        }
    }

    @Test
    public void loadClassWhenClassNotFoundByChild()
            throws Throwable {

        final URL url = new File("someMadeUpPathThatWillEnsureClassNotLoadedFromChildButDelegatedToParent")
                .toURI().toURL();

        final ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(new URL[]{url},
                this.getClass().getClassLoader());

        final Class clazz = childFirstLoader.loadClass(Dummy.class.getName());
        final Object objectFromClassLoadedByChildFirst = clazz.newInstance();

        final Dummy dummy = new Dummy();

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(),
                not(childFirstLoader));

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(),
                is(dummy.getClass().getClassLoader()));
    }

    @Test
    public void loadAndResolve()
            throws Throwable {

        final LoadMethodWithResolveParameterExposedClassLoader classLoader =
                new LoadMethodWithResolveParameterExposedClassLoader(new URL[]{}, this.getClass().getClassLoader());

        final Class clazz = classLoader.loadClass(Dummy.class.getName(), true);

        assertThat(clazz, notNullValue());
    }

    @Test
    public void securityException()
            throws ClassNotFoundException {

        final FindMethodThrowSecurityExceptionClassLoader classLoader =
                new FindMethodThrowSecurityExceptionClassLoader(new URL[]{}, this.getClass().getClassLoader());

        try {
            classLoader.loadClass(String.class.getName());

            fail("Exception was expected");
        } catch (IdentifiableException e) {
            assertThat(e.getReasonEnum(), is(SECURITY_EXCEPTION));
        }
    }

    @Test
    public void multiThreadedLoad()
            throws Throwable {

        final int numberOfInvokeAlls = 5;
        final int numberOfTasks = 100;
        final int numberOfThreads = 50;
        final URL url = new File(TARGET_TEST_CLASSES_DIRECTORY).toURI().toURL();

        final ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);

        int exceptionCounter = 0;

        for (int i = 0; i < numberOfInvokeAlls; i++) {
            final ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(new URL[]{url},
                    this.getClass().getClassLoader());

            final List<Callable<Void>> tasks = getTasks(childFirstLoader, numberOfTasks);
            final List<Future<Void>> futures = executorService.invokeAll(tasks);

            for (final Future<Void> future : futures) {
                try {
                    future.get();
                } catch (ExecutionException e) {
                    exceptionCounter++;
                }
            }
        }

        assertThat(exceptionCounter, is(0));
    }

    @SuppressWarnings("unchecked")
    private List<Callable<Void>> getTasks(final ClassLoader classLoader,
                                          int numberOfTasks) {

        final List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(numberOfTasks);

        for (int i = 0; i < numberOfTasks; i++) {
            tasks.add(new LoadClassCallable(classLoader));
        }

        return tasks;
    }

    /**
     * Dummy class to test class loading.
     */
    public static class Dummy {
    }

    public static class LoadClassCallable implements Callable {
        private ClassLoader classLoader;

        public LoadClassCallable(final ClassLoader classLoader) {
            this.classLoader = classLoader;
        }

        @Override
        public Object call()
                throws Exception {

            classLoader.loadClass(Dummy.class.getName());

            return null;
        }
    }

    public static class LoadMethodWithResolveParameterExposedClassLoader extends ChildFirstUrlClassLoader {
        /**
         * Constructor.
         *
         * @param urls        URLs from which to try and load classes.
         * @param classLoader Parent class loader.
         */
        public LoadMethodWithResolveParameterExposedClassLoader(final URL[] urls,
                                                                final ClassLoader classLoader) {

            super(urls, classLoader);
        }

        /**
         * Made public to enable further test coverage.
         *
         * @param name    Class name.
         * @param resolve Resolve the class.
         * @return class.
         * @throws ClassNotFoundException
         */
        @Override
        public synchronized Class<?> loadClass(final String name,
                                               final boolean resolve)
                throws ClassNotFoundException {

            return super.loadClass(name, resolve);
        }
    }

    public static class FindMethodThrowSecurityExceptionClassLoader extends ChildFirstUrlClassLoader {
        /**
         * Constructor.
         *
         * @param urls        URLs from which to try and load classes.
         * @param classLoader Parent class loader.
         */
        public FindMethodThrowSecurityExceptionClassLoader(final URL[] urls,
                                                           final ClassLoader classLoader) {
            super(urls, classLoader);
        }

        @Override
        protected Class<?> findClass(final String name)
                throws ClassNotFoundException {

            throw new SecurityException();
        }
    }
}
