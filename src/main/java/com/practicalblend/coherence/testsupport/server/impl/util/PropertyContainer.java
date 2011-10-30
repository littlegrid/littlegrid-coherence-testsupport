package com.practicalblend.coherence.testsupport.server.impl.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * Property container, contains a collection of keys and associated values.
 */
public class PropertyContainer {
    private Map<String, String> internalProperties = new HashMap<String, String>();

    public PropertyContainer() {
    }

    public PropertyContainer(String key,
                             String value) {

        internalProperties.put(key, value);
    }

    public PropertyContainer(PropertyContainer propertyContainer) {
        addPropertyContainer(propertyContainer);
    }

    public PropertyContainer(Properties properties) {
        addPropertyContainer(properties);
    }

    private void addPropertyContainer(Properties properties) {
        addProperties(properties);
    }

    public void addProperty(String key,
                            String value) {

        internalProperties.put(key, value);
    }

    public void addPropertyContainer(PropertyContainer propertyContainer) {
        if (propertyContainer != null) {
            for (Map.Entry<String, String> entry : propertyContainer.entrySet()) {
                addProperty(entry.getKey(), entry.getValue());
            }
        }
    }

    public void addProperties(Properties properties) {
        if (properties != null) {
            for (Map.Entry<Object, Object> entry : properties.entrySet()) {
                addProperty((String) entry.getKey(), (String) entry.getValue());
            }
        }
    }

    public Set<String> keySet() {
        return internalProperties.keySet();
    }

    public Set<Map.Entry<String, String>> entrySet() {
        return internalProperties.entrySet();
    }

    public String getProperty(String key) {
        return internalProperties.get(key);
    }

    public int size() {
        return internalProperties.size();
    }

    public Properties getProperties() {
        Properties properties = new Properties();

        for (Map.Entry<String, String> entry : internalProperties.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();

            // Hashtable cannot have a value which is null
            if (value == null) {
                value = "";
            }

            properties.setProperty(key, value);
        }

        return properties;
    }

    @Override
    public String toString() {
        return internalProperties.toString();
    }
}
