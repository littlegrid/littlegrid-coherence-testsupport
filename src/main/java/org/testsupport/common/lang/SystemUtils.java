package org.testsupport.common.lang;

import org.testsupport.coherence.impl.PropertyContainer;

import java.util.Map;
import java.util.Set;

/**
 * System utilities class providing useful system related methods.
 */
public class SystemUtils {
    /**
     * Private constructor to prevent creation.
     */
    private SystemUtils() {
    }

    /**
     * Sets, replaces or clears system properties using the supplied property container, returning a property
     * container object containing all existing system properties that have been replaced or cleared.
     *
     * @param propertyContainer Properties to be used and set and thus made available for use.
     * @return returns property container containing all existing system properties that have been replaced
     *         or cleared.
     */
    @Deprecated
    public static PropertyContainer setReplaceClearSystemProperties(PropertyContainer propertyContainer) {
        PropertyContainer replacedSystemProperties = new PropertyContainer();

        for (Map.Entry<String, String> entry : propertyContainer.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            String replacedValue = System.getProperty(key);

            if (replacedValue != null) {
                replacedSystemProperties.addProperty(key, replacedValue);
            }

            if (value == null || value.isEmpty()) {
                System.clearProperty(key);
            } else {
                System.setProperty(key, value);
            }
        }

        return replacedSystemProperties;
    }

    /**
     * Get current system properties which start with the specified prefix.
     *
     * @param prefix Prefix.
     * @return property container with system properties which start with the prefix.
     */
    @Deprecated
    public static PropertyContainer getSystemPropertiesWithPrefix(String prefix) {
        PropertyContainer prefixedPropertyContainer = new PropertyContainer();
        Set<String> keys = System.getProperties().stringPropertyNames();

        for (String key : keys) {
            if (key.contains(prefix)) {
                String value = System.getProperty(key);

                prefixedPropertyContainer.addProperty(key, value);
            }
        }

        return prefixedPropertyContainer;
    }
}
