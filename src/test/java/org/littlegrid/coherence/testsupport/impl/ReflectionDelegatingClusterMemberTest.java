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

import org.junit.Test;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Reflection delegating cluster member test to ensure all methods of delegate are
 * called as expected.
 */
public class ReflectionDelegatingClusterMemberTest {
    @Test(expected = IllegalStateException.class)
    public void noClassName() {
        final ReflectionDelegatingClusterMember delegatingClusterMember =
                new ReflectionDelegatingClusterMember(new Properties());

    }

    @Test
    public void operateDelegate() {
        final ReflectionDelegatingClusterMember delegatingClusterMember =
                new ReflectionDelegatingClusterMember(getProperties());

        delegatingClusterMember.start();
        delegatingClusterMember.shutdown();
        delegatingClusterMember.stop();
        delegatingClusterMember.getLocalMemberId();
        delegatingClusterMember.getActualContainingClassLoader();

        assertThat(delegatingClusterMember.getDelegateInstance().toString(), is("15"));
    }

    private Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("DelegateClassName",
                "org.littlegrid.coherence.testsupport.impl.ReflectionDelegatingClusterMemberTest$MockDelegateClusterMember");

        properties.setProperty("StartMethodName", "myStart");
        properties.setProperty("ShutdownMethodName", "myShutdown");
        properties.setProperty("StopMethodName", "myStop");
        properties.setProperty("GetLocalMemberIdMethodName", "myGetLocalMemberId");
        properties.setProperty("GetActualContainingClassLoaderMethodName", "myGetActualContainingClassLoader");

        return properties;
    }

    /**
     * Simple mock delegate that increments a counter each time a method is invoked, allowing
     * for the total to be checked.
     */
    public static class MockDelegateClusterMember {
        private int count;

        public void myStart() {
            count += 1;
        }

        public void myShutdown() {
            count += 2;
        }

        public void myStop() {
            count += 3;
        }

        public int myGetLocalMemberId() {
            count += 4;

            return 123;
        }

        public ClassLoader myGetActualContainingClassLoader() {
            count += 5;

            return this.getClass().getClassLoader();
        }

        @Override
        public String toString() {
            return Integer.toString(count);
        }
    }
}
