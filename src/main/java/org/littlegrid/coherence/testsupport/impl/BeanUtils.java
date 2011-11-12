package org.littlegrid.coherence.testsupport.impl;

import com.tangosol.util.ValueManipulator;
import com.tangosol.util.processor.PropertyManipulator;

import java.util.Properties;

/**
 * Bean utilities class.
 */
final class BeanUtils {
    /**
     * Private constructor to prevent creation.
     */
    private BeanUtils() {
    }

    /**
     * Invokes setter methods to set state on bean using properties as the method name and value to set.
     *
     * @param bean  Bean on which to invoke methods.
     * @param properties  Properties, keys are used for method names, whilst values are used to set state.
     * @return  number of methods invoked.
     */
    public static int processProperties(final Object bean,
                                        final Properties properties) {

        int propertiesSetCounter = 0;

        for (String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            final ValueManipulator manipulator = new PropertyManipulator(key);

            manipulator.getUpdater().update(bean, value);

            propertiesSetCounter++;
        }

        return propertiesSetCounter;
    }
}
