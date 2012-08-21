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

import java.net.URL;
import java.net.URLClassLoader;

/**
 * Child-first URL class-loader, changes the normal class-loading order by attempting
 * to load the class locally from the child before delegating to the parent.
 */
public class ChildFirstUrlClassLoader extends URLClassLoader {
    /**
     * Constructor.
     *
     * @param urls URLs from which to try and load classes.
     */
    public ChildFirstUrlClassLoader(final URL[] urls) {
        super(urls);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> findClass(String name)
            throws ClassNotFoundException {

        return super.findClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(final String name,
                                              final boolean resolve)
            throws ClassNotFoundException {

        return super.loadClass(name, resolve);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized Class<?> loadClass(final String name)
            throws ClassNotFoundException {

        Class loadedClass = findLoadedClass(name);

        if (loadedClass == null) {
            try {
                // Hasn't already been loaded, so check if this child class-loader can load the class
                loadedClass = findClass(name);
            } catch (ClassNotFoundException e) {
                // Child didn't have the class, delegate to parent class-loader
                return super.loadClass(name);
            } catch (SecurityException e) {
                throw new IllegalStateException(
                        "Please check your class path as it should not contain "
                        + "any core JAR files relating to the JRE/JDK such as rt.jar etc.  Typical reasons for this "
                        + "problem are if your JAVA_HOME environment variable is different from the JDK configured in "
                        + "your IDE or if you're using OSGI and some of the OSGI bundled JARs are being included in "
                        + "your class path: " + e);
            }
        }

        return loadedClass;
    }
}
