package org.testsupport.common.utils;

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
                // Hasn't already been loaded, so check if this child class-loader has the class
                loadedClass = findClass(name);
            } catch (ClassNotFoundException e) {
                // Child didn't have the class, delegate to parent class-loader
                return super.loadClass(name);
            }
        }

        return loadedClass;
    }
}
