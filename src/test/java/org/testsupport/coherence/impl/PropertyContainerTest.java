package org.testsupport.coherence.impl;

import org.testsupport.coherence.support.common.AbstractTestSupportTest;
import org.junit.Test;
import org.testsupport.coherence.impl.PropertyContainer;

import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Property container tests.
 */
@Deprecated
public class PropertyContainerTest extends AbstractTestSupportTest {
    private static final String KNOWN_KEY_1 = "key1";
    private static final String KNOWN_VALUE_1 = "value1";
    private static final String KNOWN_KEY_2 = "key2";
    private static final String KNOWN_VALUE_2 = "value2";

    @Test
    public void constructAndCheck() {
        PropertyContainer propertyContainer = new PropertyContainer();
        Set<String> keys = propertyContainer.keySet();
        Set<Map.Entry<String, String>> entries = propertyContainer.entrySet();

        assertThat(propertyContainer.size(), is(0));
        assertThat(keys.size(), is(0));
        assertThat(entries.size(), is(0));
    }

    @Test
    public void constructWithValueThenAddAndCheck() {
        PropertyContainer propertyContainer = new PropertyContainer(KNOWN_KEY_1, KNOWN_VALUE_1);
        propertyContainer.addProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        checkPropertyContainerPopulatedAsExpected(propertyContainer);
    }

    @Test
    public void constructWithPropertyContainerAndCheck() {
        PropertyContainer propertyContainer1 = new PropertyContainer(KNOWN_KEY_1, KNOWN_VALUE_1);
        propertyContainer1.addProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        PropertyContainer propertyContainer2 = new PropertyContainer(propertyContainer1);

        checkPropertyContainerPopulatedAsExpected(propertyContainer2);
    }

    @Test
    public void constructWithPropertiesAndCheck() {
        Properties properties = new Properties();
        properties.setProperty(KNOWN_KEY_1, KNOWN_VALUE_1);
        properties.setProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        PropertyContainer propertyContainer = new PropertyContainer(properties);

        checkPropertyContainerPopulatedAsExpected(propertyContainer);
    }

    @Test
    public void constructAddAndCheck() {
        PropertyContainer propertyContainer = new PropertyContainer();
        propertyContainer.addProperty(KNOWN_KEY_1, KNOWN_VALUE_1);
        propertyContainer.addProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        checkPropertyContainerPopulatedAsExpected(propertyContainer);
    }

    @Test
    public void getProperties() {
        PropertyContainer propertyContainer = getPopulatedPropertyContainer();
        Properties properties = propertyContainer.getProperties();

        assertThat(properties.size(), is(propertyContainer.size()));
        assertThat(properties.getProperty(KNOWN_KEY_1), is(KNOWN_VALUE_1));
        assertThat(properties.getProperty(KNOWN_KEY_2), is(KNOWN_VALUE_2));
    }

    @Test
    public void addPropertyContainer() {
        PropertyContainer propertyContainer = new PropertyContainer();
        propertyContainer.addPropertyContainer(getPopulatedPropertyContainer());

        checkPropertyContainerPopulatedAsExpected(propertyContainer);
    }

    @Test
    public void addProperties() {
        Properties properties = new Properties();
        properties.setProperty(KNOWN_KEY_1, KNOWN_VALUE_1);
        properties.setProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        PropertyContainer propertyContainer = new PropertyContainer();
        propertyContainer.addProperties(properties);

        checkPropertyContainerPopulatedAsExpected(propertyContainer);
    }

    @Test
    public void addPropertyContainerWhichIsNull() {
        PropertyContainer propertyContainer = new PropertyContainer();
        propertyContainer.addPropertyContainer(null);

        assertThat(propertyContainer.size(), is(0));
    }

    @Test
    public void addPropertyWithNullValue() {
        PropertyContainer propertyContainer = new PropertyContainer();
        propertyContainer.addProperty("key-which-has-null-value", null);

        assertThat(propertyContainer.size(), is(1));

        Properties properties = propertyContainer.getProperties();

        assertThat(properties.size(), is(1));
    }

    private PropertyContainer getPopulatedPropertyContainer() {
        PropertyContainer populatedProperties = new PropertyContainer(KNOWN_KEY_1, KNOWN_VALUE_1);
        populatedProperties.addProperty(KNOWN_KEY_2, KNOWN_VALUE_2);

        return populatedProperties;
    }

    private static void checkPropertyContainerPopulatedAsExpected(PropertyContainer propertyContainer) {
        Set<String> keys = propertyContainer.keySet();
        Set<Map.Entry<String, String>> entries = propertyContainer.entrySet();

        assertThat(propertyContainer.size(), is(2));
        assertThat(keys.size(), is(2));
        assertThat(entries.size(), is(2));

        String value1 = propertyContainer.getProperty(KNOWN_KEY_1);
        assertThat(value1, is(KNOWN_VALUE_1));

        String value2 = propertyContainer.getProperty(KNOWN_KEY_2);
        assertThat(value2, is(KNOWN_VALUE_2));
    }
}
