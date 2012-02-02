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

import com.tangosol.util.ClassHelper;
import org.littlegrid.utils.LoggerPlaceHolder;
import org.littlegrid.utils.PropertiesUtils;

import java.lang.reflect.Constructor;
import java.util.Properties;

import static java.lang.String.format;

/**
 * Reflection based cluster member, provides a declarative mechanism to delegate
 * to a class that acts as a cluster member.  In the event of that class not
 * implementing all the methods of {@link org.littlegrid.coherence.testsupport.ClusterMember}
 * then this class will simply handle them with default cluster member behaviour.
 */
public class ReflectionDelegatingClusterMember extends DefaultClusterMember {
    private static final String REFLECTION_DELEGATING_PROPERTIES_FILENAME =
            "littlegrid-reflectiondelegating-clustermember.properties";

    private static final String DELEGATE_CLASS_NAME_KEY = "DelegateClassName";
    private static final String START_METHOD_NAME_KEY = "StartMethodName";
    private static final String SHUTDOWN_METHOD_NAME_KEY = "ShutdownMethodName";
    private static final String STOP_METHOD_NAME_KEY = "StopMethodName";
    private static final String GET_LOCAL_MEMBER_ID_METHOD_NAME_KEY = "GetLocalMemberIdMethodName";
    private static final String GET_ACTUAL_CONTAINING_CLASS_LOADER_METHOD_NAME_KEY =
            "GetActualContainingClassLoaderMethodName";

    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(ReflectionDelegatingClusterMember.class.getName());

    private String delegateClassName;
    private String startMethodName;
    private String shutdownMethodName;
    private String stopMethodName;
    private String getLocalMemberIdMethodName;
    private String getActualContainingClassLoaderMethodName;
    private Object clusterMemberInstance;


    /**
     * Constructor.
     */
    public ReflectionDelegatingClusterMember() {
        this(PropertiesUtils.loadProperties(REFLECTION_DELEGATING_PROPERTIES_FILENAME));
    }

    /**
     * Constructor.
     *
     * @param properties Properties containing the mapping to the class and methods to be invoked.
     */
    public ReflectionDelegatingClusterMember(final Properties properties) {
        delegateClassName = properties.getProperty(DELEGATE_CLASS_NAME_KEY);
        startMethodName = properties.getProperty(START_METHOD_NAME_KEY);
        shutdownMethodName = properties.getProperty(SHUTDOWN_METHOD_NAME_KEY);
        stopMethodName = properties.getProperty(STOP_METHOD_NAME_KEY);
        getLocalMemberIdMethodName = properties.getProperty(GET_LOCAL_MEMBER_ID_METHOD_NAME_KEY);
        getActualContainingClassLoaderMethodName =
                properties.getProperty(GET_ACTUAL_CONTAINING_CLASS_LOADER_METHOD_NAME_KEY);

        try {
            final Class clusterMemberClass = this.getClass().getClassLoader().loadClass(delegateClassName);

            final Constructor constructor = clusterMemberClass.getConstructor();

            clusterMemberInstance = constructor.newInstance();
        } catch (Exception e) {
            LOGGER.warn(format("Cannot create instance of '%s', will use default behaviour", delegateClassName));
        }
    }

    @Override
    public void start() {
        try {
            ClassHelper.invoke(clusterMemberInstance, startMethodName, new Object[]{});
        } catch (Exception e) {
            super.start();
        }
    }

    @Override
    public void shutdown() {
        try {
            ClassHelper.invoke(clusterMemberInstance, shutdownMethodName, new Object[]{});
        } catch (Exception e) {
            super.shutdown();
        }
    }

    @Override
    public void stop() {
        try {
            ClassHelper.invoke(clusterMemberInstance, stopMethodName, new Object[]{});
        } catch (Exception e) {
            super.stop();
        }
    }

    @Override
    public int getLocalMemberId() {
        try {
            return (Integer) ClassHelper.invoke(clusterMemberInstance, getLocalMemberIdMethodName, new Object[]{});
        } catch (Exception e) {
            return super.getLocalMemberId();
        }
    }

    @Override
    public ClassLoader getActualContainingClassLoader() {
        try {
            return (ClassLoader) ClassHelper.invoke(clusterMemberInstance, getActualContainingClassLoaderMethodName,
                    new Object[]{});

        } catch (Exception e) {
            return super.getActualContainingClassLoader();
        }
    }
}
