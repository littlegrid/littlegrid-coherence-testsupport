package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMember;
import org.testsupport.common.LoggerWrapper;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import static java.lang.String.format;

/**
 * Delegating cluster member wrapper, loads a class that implements {@link ClusterMember}
 * into a separate class loader and then delegates requests (start, stop, shutdown etc.) to
 * the instance of the wrapped class.
 */
class ClusterMemberDelegatingWrapper implements ClusterMember {
    private LoggerWrapper logger = new LoggerWrapper(ClusterMemberDelegatingWrapper.class.getName());
    private Object clusterMemberInstance;

    /**
     * Constructor.
     *
     * @param clusterMemberInstanceClassName Name of class to instantiate and delegate calls to.
     * @param childFirstUrlClassLoader       Instance of child first class loader.
     */
    public ClusterMemberDelegatingWrapper(final String clusterMemberInstanceClassName,
                                          final ChildFirstUrlClassLoader childFirstUrlClassLoader) {
        try {
            logger.fine(format("Cluster member class to be instantiated: '%s'", clusterMemberInstanceClassName));

            Class clusterMemberClass = childFirstUrlClassLoader.loadClass(clusterMemberInstanceClassName);
            Constructor constructor = clusterMemberClass.getConstructor();
            clusterMemberInstance = constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void start() {
        logger.fine("About to start this cluster member");

        invokeMethod(clusterMemberInstance, "start");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        logger.fine("Shutting down this cluster member");

        invokeMethod(clusterMemberInstance, "shutdown");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        logger.fine("Stopping this cluster member");

        invokeMethod(clusterMemberInstance, "stop");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLocalMemberId() {
        return (Integer) invokeMethod(clusterMemberInstance, "getLocalMemberId");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClassLoader getActualContainingClassLoader() {
        return (ClassLoader) invokeMethod(clusterMemberInstance, "getActualContainingClassLoader");
    }

    private Object invokeMethod(Object objectToInvokeMethodOn,
                                String methodName) {

        try {
            Method method = objectToInvokeMethodOn.getClass().getDeclaredMethod(methodName, new Class[]{});

            return method.invoke(objectToInvokeMethodOn);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }
}
