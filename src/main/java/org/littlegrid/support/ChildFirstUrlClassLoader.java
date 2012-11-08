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

import org.littlegrid.IdentifiableException;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;
import static org.littlegrid.IdentifiableException.ReasonEnum.SECURITY_EXCEPTION;

/**
 * Child-first URL class-loader, changes the normal class-loading order by attempting
 * to load the class locally from the child before delegating to the parent.
 */
public class ChildFirstUrlClassLoader extends URLClassLoader {
    private Map<String, URL> loadedResources = new HashMap<String, URL>();

    /**
     * Constructor.
     *
     * @param urls        URLs from which to try and load classes.
     * @param classLoader Parent class loader.
     */
    public ChildFirstUrlClassLoader(final URL[] urls,
                                    final ClassLoader classLoader) {

        super(urls, classLoader);
    }

    /**
     * {@inheritDoc}
     *
     * @since 2.11
     */
    @Override
    public synchronized URL getResource(final String name) {
        URL loadResource = loadedResources.get(name);

        if (loadResource == null) {
            // Hasn't already been loaded, so check if this child class-loader can load the resource
            loadResource = findResource(name);

            if (loadResource == null) {
                // Child didn't have the resource, delegate to parent class-loader mechanism
                loadResource = super.getResource(name);
            } else {
                loadedResources.put(name, loadResource);
            }
        }

        return loadResource;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(final String name,
                                              final boolean resolve)
            throws ClassNotFoundException {

        Class loadedClass = null;

        loadedClass = findLoadedClass(name);

        if (loadedClass == null) {
            try {
                // Hasn't already been loaded, so check if this child class-loader can load the class
                loadedClass = findClass(name);
            } catch (ClassNotFoundException e) {
                // Child didn't have the class, delegate to parent class-loader
                loadedClass = getParent().loadClass(name);
            } catch (SecurityException e) {
                throw new IdentifiableException(
                        format("Cannot load '%s' , please check your class path as it "
                                + "should not contain any core JAR files relating to the JRE/JDK such "
                                + "as rt.jar etc.  Typical reasons for this problem are if your JAVA_HOME "
                                + "environment variable is different from the JDK configured in your IDE "
                                + "or if you're using OSGI and some of the OSGI bundled JARs are being "
                                + "included in your class path: '%s'",
                                name, e),
                        SECURITY_EXCEPTION);
            }
        }

        if (resolve) {
            resolveClass(loadedClass);
        }

        return loadedClass;
    }
}
