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

package org.littlegrid.utils;

import org.junit.Test;

import java.io.File;
import java.net.URL;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

/**
 * Child first URL class loader tests.
 */
public final class ChildFirstUrlClassLoaderTest {
    private static final String CLASS_TO_TRY_AND_LOAD = "org.littlegrid.utils.ChildFirstUrlClassLoaderTest$Dummy";

    @Test
    public void loadClassFromChildClassLoader()
            throws Throwable {

        URL url = new File("./target/test-classes").toURI().toURL();
        ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(new URL[]{url});
        Class clazz = childFirstLoader.loadClass(CLASS_TO_TRY_AND_LOAD);

        Object objectFromClassLoadedByChildFirst = clazz.newInstance();
        Dummy dummy = new Dummy();

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), is(childFirstLoader));
        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), not(dummy.getClass().getClassLoader()));
    }

    @Test
    public void loadClassWhenClassNotFound()
            throws Throwable {

        URL url = new File("someMadeUpPathThatWillEnsureClassNotLoadedFromChildButDelegatedToParent").toURI().toURL();
        ClassLoader childFirstLoader = new ChildFirstUrlClassLoader(new URL[]{url});
        Class clazz = childFirstLoader.loadClass(CLASS_TO_TRY_AND_LOAD);

        Object objectFromClassLoadedByChildFirst = clazz.newInstance();
        Dummy dummy = new Dummy();

        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), not(childFirstLoader));
        assertThat(objectFromClassLoadedByChildFirst.getClass().getClassLoader(), is(dummy.getClass().getClassLoader()));
    }

    /**
     * Dummy class to test class loading.
     */
    public static class Dummy {
    }
}
