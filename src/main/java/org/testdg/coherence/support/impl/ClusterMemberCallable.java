package org.testdg.coherence.support.impl;

import java.net.URL;
import java.util.concurrent.Callable;

/**
 * Local process cluster member callable, a task that starts a cluster member wrapper.
 */
class ClusterMemberCallable implements Callable<ClusterMemberDelegatingWrapper> {
    private URL[] classPathUrls;
    private String clusterMemberInstanceClassName;

    /**
     * Constructor.
     *
     * @param clusterMemberInstanceClassName  Cluster member class name.
     * @param classPathUrls  Class path.
     */
    public ClusterMemberCallable(final String clusterMemberInstanceClassName,
                                 final URL[] classPathUrls) {

        if (clusterMemberInstanceClassName == null) {
            throw new IllegalStateException("Cluster member class name cannot be null");
        }

        if (classPathUrls == null) {
            throw new IllegalStateException("Class path URLs cannot be null");
        }

        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;
        this.classPathUrls = classPathUrls;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberDelegatingWrapper call()
            throws Exception {

        ClassLoader originalClassLoader = Thread.currentThread().getContextClassLoader();

        try {
            ChildFirstUrlClassLoader childFirstUrlClassLoader = new ChildFirstUrlClassLoader(classPathUrls);
            Thread.currentThread().setContextClassLoader(childFirstUrlClassLoader);

            ClusterMemberDelegatingWrapper memberWrapper =
                    new ClusterMemberDelegatingWrapper(clusterMemberInstanceClassName, childFirstUrlClassLoader);
            memberWrapper.start();

            return memberWrapper;
        } catch (Throwable throwable) {
            throw new Exception(throwable);
        } finally {
            Thread.currentThread().setContextClassLoader(originalClassLoader);
        }
    }
}
