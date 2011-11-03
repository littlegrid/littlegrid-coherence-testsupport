package org.testsupport.coherence.impl;

import org.testsupport.common.net.ChildFirstUrlClassLoader;

import java.net.URL;
import java.util.concurrent.Callable;

/**
 * Local process cluster member callable, a task that starts a cluster member wrapper.
 */
class ClusterMemberCallable implements Callable<ClusterMemberDelegatingWrapper> {
    private URL[] classPathUrls;
    private String clusterMemberClassName;

    /**
     * Constructor.
     *
     * @param clusterMemberClassName  Cluster member class name.
     * @param classPathUrls  Class path.
     */
    public ClusterMemberCallable(String clusterMemberClassName,
                                 URL[] classPathUrls) {

        if (clusterMemberClassName == null) {
            throw new IllegalStateException("Cluster member class name cannot be null");
        }

        if (classPathUrls == null) {
            throw new IllegalStateException("Class path URLs cannot be null");
        }

        this.clusterMemberClassName = clusterMemberClassName;
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
                    new ClusterMemberDelegatingWrapper(clusterMemberClassName, childFirstUrlClassLoader);
            memberWrapper.start();

            return memberWrapper;
        } catch (Throwable throwable) {
            throw new Exception(throwable);
        } finally {
            Thread.currentThread().setContextClassLoader(originalClassLoader);
        }
    }
}
