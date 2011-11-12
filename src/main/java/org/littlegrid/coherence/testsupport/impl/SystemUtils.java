package org.littlegrid.coherence.testsupport.impl;

import java.util.Properties;

/**
 * System utilities class providing useful system related methods.
 */
final class SystemUtils {
    /**
     * Private constructor to prevent creation.
     */
    private SystemUtils() {
    }

    /**
     * Captures current system properties.
     *
     * @return current properties.
     */
    public static Properties snapshotSystemProperties() {
        Properties properties = new Properties();

        for (String key : System.getProperties().stringPropertyNames()) {
            String value = System.getProperty(key);

            properties.setProperty(key, value);
        }

        return properties;
    }

    /**
     * Apply the properties to the system properties.
     *
     * @param properties New and updated system properties.
     */
    public static void applyToSystemProperties(final Properties properties) {
        for (String key : properties.stringPropertyNames()) {
            String value = properties.getProperty(key);

            if (!value.trim().isEmpty()) {
                System.setProperty(key, value);
            }
        }
    }

    /**
     * Get current system properties which start with the specified prefix.
     *
     * @param prefix Prefix.
     * @return properties.
     */
    public static Properties getSystemPropertiesWithPrefix(final String prefix) {
        Properties prefixedProperties = new Properties();

        for (String key : System.getProperties().stringPropertyNames()) {
            if (key.contains(prefix)) {
                String value = System.getProperty(key);

                prefixedProperties.setProperty(key, value);
            }
        }

        return prefixedProperties;
    }
}
