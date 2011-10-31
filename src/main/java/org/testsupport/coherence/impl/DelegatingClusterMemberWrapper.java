package org.testsupport.coherence.impl;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Member;
import org.apache.log4j.Logger;
import org.testsupport.common.net.ChildFirstUrlClassLoader;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import static java.lang.String.format;

/**
 * Delegating cluster member wrapper, loads a class that implements {@link ClusterMember} into a separate
 * class loader and then delegates requests (start, stop, shutdown etc.) to the instance of the wrapped
 * class.
 */
class DelegatingClusterMemberWrapper implements ClusterMember {
    private Logger logger = Logger.getLogger(this.getClass());
    private Object clusterMemberInstance;

    public DelegatingClusterMemberWrapper(String clusterMemberClassName,
                                          ChildFirstUrlClassLoader childFirstUrlClassLoader) {
        try {
//            logger.debug(format("Cluster member class to be instantiated: '%s'", clusterMemberClassName));
            CacheFactory.log(format("****Cluster member class to be instantiated: '%s'", clusterMemberClassName));

            Class clusterMemberClass = childFirstUrlClassLoader.loadClass(clusterMemberClassName);
            Constructor constructor = clusterMemberClass.getConstructor();
            clusterMemberInstance = constructor.newInstance();
        } catch (Exception e) {
            logger.error(e, e);
            throw new IllegalStateException(e);
        }
    }

    public void start() {
        CacheFactory.log("About to start this cluster member");
//        logger.debug("About to start this cluster member");

        invokeMethod(clusterMemberInstance, "start");
    }

    public void shutdown() {
        CacheFactory.log("Shutting down this cluster member");
//        logger.debug("Shutting down this cluster member");

        invokeMethod(clusterMemberInstance, "shutdown");
    }

    public void stop() {
        logger.debug("Stopping this cluster member");

        invokeMethod(clusterMemberInstance, "stop");
    }

    public int getLocalMemberId() {
        return (Integer) invokeMethod(clusterMemberInstance, "getLocalMemberId");
    }

    @Override
    public Member getLocalMember() {
        throw new UnsupportedOperationException();
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
