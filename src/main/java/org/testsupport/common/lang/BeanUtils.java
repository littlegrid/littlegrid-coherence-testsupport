package org.testsupport.common.lang;

import com.tangosol.util.ValueManipulator;
import com.tangosol.util.processor.PropertyManipulator;

import java.util.Properties;

/**
 * Bean utilities class.
 */
public final class BeanUtils {
    public static int applyProperties(final Object bean,
                                      final Properties properties) {

        int propertiesSetCounter = 0;

        for (String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            final ValueManipulator manipulator = new PropertyManipulator(key);

            try {
                manipulator.getUpdater().update(bean, value);
            } catch (RuntimeException e) {
                //TODO: This is a bit rough but is functional for now
                manipulator.getUpdater().update(bean, Integer.parseInt(value));
            }

            propertiesSetCounter++;
        }

        return propertiesSetCounter;
    }
}
