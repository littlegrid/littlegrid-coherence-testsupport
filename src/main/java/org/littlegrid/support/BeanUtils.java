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

package org.littlegrid.support;

import com.tangosol.util.ClassHelper;
import org.littlegrid.IdentifiableException;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static java.lang.String.format;
import static org.littlegrid.IdentifiableException.ReasonEnum.UNABLE_TO_SET_BEAN_PROPERTY;

/**
 * Bean utilities class.
 */
public final class BeanUtils {
    private static final String SET_PREFIX = "set";

    /**
     * Default scope to enable test coverage.
     */
    BeanUtils() {
        throw new UnsupportedOperationException();
    }

    /**
     * Invokes setter methods to set state on bean using properties with the key being the method
     * name (without set) and the value to being what it should be set to.
     *
     * @param bean       Bean on which to invoke methods.
     * @param properties Properties, keys are used for method names, whilst values are used to set state.
     * @return number of methods invoked.
     */
    public static int multiSetter(final Object bean,
                                  final Properties properties) {

        final Map<String, String> methodNameMapping = new HashMap<String, String>();

        final Method[] methods = bean.getClass().getDeclaredMethods();

        for (final Method method : methods) {
            final String methodName = method.getName();

            methodNameMapping.put(methodName.toUpperCase(), methodName);
        }

        final Map<String, String> propertyNameMapping = new HashMap<String, String>(properties.size());

        for (final Object key : properties.keySet()) {
            final String methodName = SET_PREFIX + key.toString();

            propertyNameMapping.put(methodName.toUpperCase(), methodName);
        }

        final Set<String> upperCasePropertyNamesNotMatched = new HashSet<String>(propertyNameMapping.keySet());
        upperCasePropertyNamesNotMatched.removeAll(methodNameMapping.keySet());

        if (upperCasePropertyNamesNotMatched.size() > 0) {
            final Set<String> mixedCasePropertyNamesNotMatched =
                    new HashSet<String>(upperCasePropertyNamesNotMatched.size());

            for (final String key : upperCasePropertyNamesNotMatched) {
                mixedCasePropertyNamesNotMatched.add(propertyNameMapping.get(key));
            }

            throw new IdentifiableException(
                    format("Following methods could not be found on the bean: %s", mixedCasePropertyNamesNotMatched),
                    UNABLE_TO_SET_BEAN_PROPERTY);
        }

        int propertiesSetCounter = 0;

        for (final Object key : properties.keySet()) {
            final String value = properties.getProperty((String) key);

            // Use the mapped real method name rather than the property name
            final String methodName = methodNameMapping.get(SET_PREFIX.toUpperCase() + key.toString().toUpperCase());

            try {
                // Try invoking with string as parameter
                ClassHelper.invoke(bean, methodName, new Object[]{value});
            } catch (Exception e) {
                try {
                    // Try invoking with an int as a parameter
                    ClassHelper.invoke(bean, methodName, new Object[]{Integer.parseInt(value)});
                } catch (Exception e2) {
                    try {
                        // Try invoking with a long as a parameter
                        ClassHelper.invoke(bean, methodName, new Object[]{Long.parseLong(value)});
                    } catch (Exception e3) {
                        try {
                            // Try invoking with a string array as a parameter
                            ClassHelper.invoke(bean, methodName, new Object[]{new String[]{value}});
                        } catch (Exception e4) {
                            throw new IdentifiableException(
                                    format("Unable to invoke '%s' to set value to: '%s' due to: %s "
                                            + "- parameter type not supported",
                                            methodName, value, e),
                                    UNABLE_TO_SET_BEAN_PROPERTY);
                        }
                    }
                }
            }

            propertiesSetCounter++;
        }

        return propertiesSetCounter;
    }
}
