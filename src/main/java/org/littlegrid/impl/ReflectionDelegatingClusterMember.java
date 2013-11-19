/*
 * Copyright (c) 2010-2013 Jonathan Hall.
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

package org.littlegrid.impl;

import com.tangosol.util.ClassHelper;
import org.littlegrid.support.PropertiesUtils;

import java.lang.reflect.Constructor;
import java.util.Properties;
import java.util.logging.Level;

import static java.lang.String.format;

/**
 * Reflection based cluster member, provides a declarative mechanism to delegate
 * to a class that acts as a cluster member - in the event of the delegate class not
 * implementing all the methods of {@link org.littlegrid.ClusterMemberGroup.ClusterMember}
 * then this class will simply handle them with default cluster member behaviour.
 * <p/>
 * The configuration of delegate class and the method mapping should be defined in
 * a properties file called <code>littlegrid-reflection-delegating.properties</code> and located in
 * the root of the JAR or within a directory within the JAR called <code>littlegrid</code>, for
 * example <code>littlegrid/littlegrid-reflection-delegating.properties</code>
 */
public class ReflectionDelegatingClusterMember extends DefaultClusterMember {
    private static final String REFLECTION_DELEGATING_PROPERTIES_FILENAME =
            "littlegrid-reflection-delegating.properties";

    private static final String DELEGATE_INSTANCE_CLASS_NAME = "DelegateInstanceClassName";
    private static final String START_METHOD_NAME_KEY = "StartMethodName";
    private static final String SHUTDOWN_METHOD_NAME_KEY = "ShutdownMethodName";
    private static final String STOP_METHOD_NAME_KEY = "StopMethodName";
    private static final String GET_LOCAL_MEMBER_ID_METHOD_NAME_KEY = "GetLocalMemberIdMethodName";
    private static final String GET_ACTUAL_CONTAINING_CLASS_LOADER_METHOD_NAME_KEY =
            "GetActualContainingClassLoaderMethodName";

    private Object delegateInstance;
    private String delegateInstanceClassName;
    private String startMethodName;
    private String shutdownMethodName;
    private String stopMethodName;
    private String getLocalMemberIdMethodName;
    private String getActualContainingClassLoaderMethodName;


    /**
     * Constructor.
     */
    public ReflectionDelegatingClusterMember() {
        loadAndInitialise();
    }

    /**
     * Constructor.
     */
    public ReflectionDelegatingClusterMember(final String delegateInstanceClassName) {
        throw new UnsupportedOperationException();
    }

    private void loadAndInitialise() {
        final Properties properties = PropertiesUtils.loadProperties(Level.INFO,
                REFLECTION_DELEGATING_PROPERTIES_FILENAME
                        + ", littlegrid/" + REFLECTION_DELEGATING_PROPERTIES_FILENAME);

        delegateInstanceClassName = properties.getProperty(DELEGATE_INSTANCE_CLASS_NAME);
        startMethodName = properties.getProperty(START_METHOD_NAME_KEY);
        shutdownMethodName = properties.getProperty(SHUTDOWN_METHOD_NAME_KEY);
        stopMethodName = properties.getProperty(STOP_METHOD_NAME_KEY);
        getLocalMemberIdMethodName = properties.getProperty(GET_LOCAL_MEMBER_ID_METHOD_NAME_KEY);
        getActualContainingClassLoaderMethodName =
                properties.getProperty(GET_ACTUAL_CONTAINING_CLASS_LOADER_METHOD_NAME_KEY);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void start() {
        try {
            final Class clusterMemberClass = this.getClass().getClassLoader().loadClass(delegateInstanceClassName);

            @SuppressWarnings("unchecked")
            final Constructor constructor = clusterMemberClass.getConstructor();

            delegateInstance = constructor.newInstance();
        } catch (Exception e) {
            throw new IllegalStateException(format("Cannot create instance of '%s', exception: %s",
                    delegateInstanceClassName, e));
        }

        try {
            ClassHelper.invoke(delegateInstance, startMethodName, new Object[]{});
        } catch (Exception e) {
            super.start();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdown() {
        try {
            ClassHelper.invoke(delegateInstance, shutdownMethodName, new Object[]{});
        } catch (Exception e) {
            super.shutdown();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        try {
            ClassHelper.invoke(delegateInstance, stopMethodName, new Object[]{});
        } catch (Exception e) {
            super.stop();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLocalMemberId() {
        try {
            return (Integer) ClassHelper.invoke(delegateInstance, getLocalMemberIdMethodName, new Object[]{});
        } catch (Exception e) {
            return super.getLocalMemberId();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClassLoader getActualContainingClassLoader() {
        try {
            return (ClassLoader) ClassHelper.invoke(delegateInstance, getActualContainingClassLoaderMethodName,
                    new Object[]{});

        } catch (Exception e) {
            return super.getActualContainingClassLoader();
        }
    }

    /**
     * Returns the object that the reflection delegating object is delegating the
     * behaviour to.
     *
     * @return delegate.
     */
    public Object getDelegateInstance() {
        return delegateInstance;
    }

    /**
     * Sets start method name.
     *
     * @param startMethodName Method name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setStartMethodName(final String startMethodName) {
        this.startMethodName = startMethodName;

        return this;
    }

    /**
     * Sets shutdown method name.
     *
     * @param shutdownMethodName Method name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setShutdownMethodName(final String shutdownMethodName) {
        this.shutdownMethodName = shutdownMethodName;

        return this;
    }

    /**
     * Sets stop method name.
     *
     * @param stopMethodName Method name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setStopMethodName(final String stopMethodName) {
        this.stopMethodName = stopMethodName;

        return this;
    }

    /**
     * Sets method name of get local member.
     *
     * @param getLocalMemberIdMethodName Method name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setGetLocalMemberIdMethodName(final String getLocalMemberIdMethodName) {
        this.getLocalMemberIdMethodName = getLocalMemberIdMethodName;

        return this;
    }

    /**
     * Set method name of get actual containing class loader.
     *
     * @param getActualContainingClassLoaderMethodName
     *         Method name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setGetActualContainingClassLoaderMethodName(
            final String getActualContainingClassLoaderMethodName) {

        this.getActualContainingClassLoaderMethodName = getActualContainingClassLoaderMethodName;

        return this;
    }

    /**
     * Sets delegate instance class name.
     *
     * @param delegateInstanceClassName Class name.
     * @return reflection delegating cluster member.
     */
    public ReflectionDelegatingClusterMember setDelegateInstanceClassName(final String delegateInstanceClassName) {
        this.delegateInstanceClassName = delegateInstanceClassName;

        return this;
    }
}
