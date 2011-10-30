package com.practicalblend.coherence.testsupport.server.impl.util;

import org.apache.log4j.Logger;

import java.util.Map;
import java.util.Set;

import static java.lang.String.format;

/**
 * System utilities class providing useful system related methods.
 */
public class SystemUtils {
    private static final Logger LOGGER = Logger.getLogger(SystemUtils.class);


    /**
     * Private constructor to prevent creation.
     */
    private SystemUtils() {
    }

    /**
     * Get value for the specified environment variable.
     *
     * @param environmentVariableName Environment variable.
     * @return value relating to environment variable.
     */
    public static String getEnvironmentVariable(String environmentVariableName) {
        String value = System.getenv(environmentVariableName);

        if (value == null) {
            String message = format("Configuration error, environment variable '%s' not set", environmentVariableName);

            LOGGER.error(message);
            System.err.println(message);

            throw new IllegalStateException(message);
        }

        return value;
    }

    /**
     * Sets, replaces or clears system properties using the supplied property container, returning a property
     * container object containing all existing system properties that have been replaced or cleared.
     *
     * @param propertyContainer Properties to be used and set and thus made available for use.
     * @return returns property container containing all existing system properties that have been replaced
     *         or cleared.
     */
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
