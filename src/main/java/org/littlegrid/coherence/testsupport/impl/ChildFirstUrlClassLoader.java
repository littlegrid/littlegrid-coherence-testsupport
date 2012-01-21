/*
 * Copyright (c) 2011, Jonathan Hall.
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

import java.net.URL;
import java.net.URLClassLoader;

/**
 * Child-first URL class-loader, changes the normal class-loading order by attempting
 * to load the class locally from the child before delegating to the parent.
 */
class ChildFirstUrlClassLoader extends URLClassLoader {
    /**
     * Constructor.
     *
     * @param urls URLs from which to try and load classes.
     * @throws Throwable Problem with construction.
     */
    public ChildFirstUrlClassLoader(final URL[] urls)
            throws Throwable {

        super(urls);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<?> loadClass(final String name)
            throws ClassNotFoundException {

        Class loadedClass = findLoadedClass(name);

        if (loadedClass == null) {
            try {
                // Hasn't already been loaded, so check if this child class-loader can load the class
                loadedClass = findClass(name);
            } catch (ClassNotFoundException e) {
                // Child didn't have the class, delegate to parent class-loader
                return super.loadClass(name);
            }
        }

        return loadedClass;
    }
}
