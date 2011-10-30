package com.practicalblend.coherence.testsupport.server.impl;

import com.practicalblend.coherence.testsupport.server.ClusterMember;
import com.practicalblend.coherence.testsupport.server.ClusterMemberGroupRuntimeException;
import com.practicalblend.coherence.testsupport.server.impl.net.ChildFirstUrlClassLoader;
import org.apache.log4j.Logger;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * Delegating cluster member wrapper, loads a class that implements {@link ClusterMember} into a separate
 * class loader and then delegates requests (start, stop, shutdown etc.) to the instance of the wrapped
 * class.
 */
public class DelegatingClusterMemberWrapper implements ClusterMember {
    private Logger logger = Logger.getLogger(this.getClass());
    private Object clusterMemberInstance;

    public DelegatingClusterMemberWrapper(String clusterMemberClassName,
                                          ChildFirstUrlClassLoader childFirstUrlClassLoader) {
        try {
            logger.debug(String.format("Cluster member class to be instantiated: '%s'", clusterMemberClassName));

            Class clusterMemberClass = childFirstUrlClassLoader.loadClass(clusterMemberClassName);
            Constructor constructor = clusterMemberClass.getConstructor();
            clusterMemberInstance = constructor.newInstance();
        } catch (Exception e) {
            logger.error(e, e);
            throw new ClusterMemberGroupRuntimeException(e);
        }
    }

    public void start() {
        logger.debug("About to start this cluster member");

        invokeMethod(clusterMemberInstance, "start");
    }

    public void shutdown() {
        logger.debug("Shutting down this cluster member");

        invokeMethod(clusterMemberInstance, "shutdown");
    }

    public void stop() {
        logger.debug("Stopping this cluster member");

        invokeMethod(clusterMemberInstance, "stop");
    }

    public int getLocalMemberId() {
        return (Integer) invokeMethod(clusterMemberInstance, "getLocalMemberId");
    }

    private Object invokeMethod(Object objectToInvokeMethodOn,
                                String methodName) {

        try {
            Method method = objectToInvokeMethodOn.getClass().getDeclaredMethod(methodName, new Class[]{});

            return method.invoke(objectToInvokeMethodOn);
        } catch (Exception e) {
            throw new ClusterMemberGroupRuntimeException(e);
        }
    }
}
