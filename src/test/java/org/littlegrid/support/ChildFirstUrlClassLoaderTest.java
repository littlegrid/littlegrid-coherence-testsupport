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

import org.junit.Test;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

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
    public void delegateWhenLoadingCoreClass()
            throws Throwable {

        final ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(
                ClassPathUtils.getClassPathUrlsExcludingJavaHome("made-up-java-home-so-real-one-will-not-be-excluded",
                        ClassPathUtils.getClassPath(System.getProperties()),
                        ClassPathUtils.getPathSeparator(System.getProperties()),
                        null),
                this.getClass().getClassLoader());

        childFirstLoader.loadClass(String.class.getName());
        childFirstLoader.loadClass(Map.class.getName());
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

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), not(childFirstLoader));
        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(),
                is(dummy.getClass().getClassLoader()));
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
}
