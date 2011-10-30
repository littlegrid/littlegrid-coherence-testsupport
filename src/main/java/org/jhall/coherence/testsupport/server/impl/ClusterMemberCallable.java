package org.jhall.coherence.testsupport.server.impl;

import org.jhall.coherence.testsupport.server.impl.net.ChildFirstUrlClassLoader;

import java.net.URL;
import java.util.concurrent.Callable;

/**
 * Local process cluster member callable, a task that starts a cluster member wrapper.
 */
public class ClusterMemberCallable implements Callable<DelegatingClusterMemberWrapper> {
    private URL[] classPathUrls;
    private String clusterMemberClassName;

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

    public DelegatingClusterMemberWrapper call()
            throws Exception {

        ClassLoader originalClassLoader = Thread.currentThread().getContextClassLoader();

        try {
            ChildFirstUrlClassLoader childFirstUrlClassLoader = new ChildFirstUrlClassLoader(classPathUrls);
            Thread.currentThread().setContextClassLoader(childFirstUrlClassLoader);

            DelegatingClusterMemberWrapper wrapper = new DelegatingClusterMemberWrapper(clusterMemberClassName, childFirstUrlClassLoader);
            wrapper.start();

            return wrapper;
        } catch (Throwable throwable) {
            throw new Exception(throwable);
        } finally {
            Thread.currentThread().setContextClassLoader(originalClassLoader);
        }
    }
}
