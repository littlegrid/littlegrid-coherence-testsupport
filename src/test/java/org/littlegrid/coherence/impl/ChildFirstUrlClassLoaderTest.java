package org.littlegrid.coherence.impl;

import org.junit.Test;
import org.littlegrid.coherence.impl.ChildFirstUrlClassLoader;
import org.littlegrid.common.AbstractTest;

import java.io.File;
import java.net.URL;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

/**
 * Child first URL class loader tests.
 */
public class ChildFirstUrlClassLoaderTest extends AbstractTest {
    private static final String CLASS_TO_TRY_AND_LOAD =
            "org.littlegrid.coherence.impl.ChildFirstUrlClassLoaderTest$Dummy";

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
