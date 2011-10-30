package org.jhall.coherence.testsupport.server.impl.util;

import org.jhall.coherence.testsupport.common.AbstractTestSupportTest;
import org.junit.Test;

import java.util.Map;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.jhall.coherence.testsupport.server.impl.util.SystemUtils.getEnvironmentVariable;
import static org.jhall.coherence.testsupport.server.impl.util.SystemUtils.getSystemPropertiesWithPrefix;
import static org.jhall.coherence.testsupport.server.impl.util.SystemUtils.setReplaceClearSystemProperties;
import static org.junit.Assert.assertThat;

/**
 * System utils tests.
 */
public class SystemUtilsTest extends AbstractTestSupportTest {
    private static final String KNOWN_PREFIX = "prefix.used.for.testing.";
    private static final String KNOWN_KEY_1 = KNOWN_PREFIX + "key1";
    private static final String KNOWN_VALUE_1 = "value1";
    private static final String KNOWN_KEY_2 = KNOWN_PREFIX + "key2";
    private static final String KNOWN_VALUE_2 = "value2";

    @Test
    public void getEnvironmentVariableJavaHomeAsItShouldExist() {
        String javaHome = getEnvironmentVariable("JAVA_HOME");

        assertThat(javaHome, notNullValue());
        assertThat(javaHome, not(""));
    }

    @Test(expected = IllegalStateException.class)
    public void getEnvironmentVariableUsingNameThatShouldNotExist() {
        getEnvironmentVariable("_ENVIRONMENT_VARIABLE_ZZZ_THAT_YYY_SHOULD_111_NOT_EXIST");
    }

    @Test
    public void setSystemPropertiesWhenItDoesNotAlreadyExist() {
        clearSystemProperties();

        String key = KNOWN_KEY_1;
        String expectedValue = KNOWN_VALUE_1;

        PropertyContainer replacedSystemProperties =
                setReplaceClearSystemProperties(new PropertyContainer(key, expectedValue));

        assertThat(System.getProperty(key), is(expectedValue));
        assertThat(replacedSystemProperties.size(), is(0));
    }

    @Test
    public void replaceSystemPropertiesWhenItDoesExist() {
        clearSystemProperties();

        String key = KNOWN_KEY_1;
        String originalValue = KNOWN_VALUE_1;
        String expectedNewValue = KNOWN_VALUE_2;

        System.setProperty(key, originalValue);

        PropertyContainer replacedSystemProperties =
                setReplaceClearSystemProperties(new PropertyContainer(key, expectedNewValue));

        assertThat(System.getProperty(key), is(expectedNewValue));
        assertThat(replacedSystemProperties.size(), is(1));

        Map.Entry<String, String> entry = getFirstEntry(replacedSystemProperties);

        assertThat(entry.getKey(), is(key));
        assertThat(entry.getValue(), is(originalValue));
    }

    @Test
    public void setAndReplaceSystemPropertiesWhenOneExistsAndOneDoesNot() {
        clearSystemProperties();

        String key = KNOWN_KEY_2;
        String originalValue = KNOWN_VALUE_2;
        String expectedNewValue = "value2.2";

        System.setProperty(key, originalValue);

        PropertyContainer propertyContainer = new PropertyContainer(KNOWN_KEY_1, KNOWN_VALUE_1);
        propertyContainer.addProperty(key, expectedNewValue);

        PropertyContainer replacedSystemProperties = setReplaceClearSystemProperties(propertyContainer);

        assertThat(System.getProperty(key), is(expectedNewValue));
        assertThat(replacedSystemProperties.size(), is(1));

        Map.Entry<String, String> entry = getFirstEntry(replacedSystemProperties);

        assertThat(entry.getKey(), is(key));
        assertThat(entry.getValue(), is(originalValue));
    }

    @Test
    public void clearSystemPropertiesWhenOneExistsAndOneDoesNot() {
        clearSystemProperties();

        String key = KNOWN_KEY_1;
        String valueThatShouldBeCleared = "this should be cleared";

        System.setProperty(key, valueThatShouldBeCleared);

        PropertyContainer propertyContainer = new PropertyContainer(KNOWN_KEY_1, "");
        propertyContainer.addProperty(KNOWN_KEY_2, "");

        PropertyContainer replacedSystemProperties = setReplaceClearSystemProperties(propertyContainer);

        assertThat(System.getProperty(key), nullValue());
        assertThat(replacedSystemProperties.size(), is(1));

        Map.Entry<String, String> entry = getFirstEntry(replacedSystemProperties);

        assertThat(entry.getKey(), is(key));
        assertThat(entry.getValue(), is(valueThatShouldBeCleared));
    }

    @Test
    public void setAndReplaceSystemPropertiesWhenPropertyContainerHasEntryWithNullValue() {
        PropertyContainer propertyContainerWithNullValue = new PropertyContainer("key-which-has-null-value", null);

        PropertyContainer replacedSystemProperties =
                setReplaceClearSystemProperties(propertyContainerWithNullValue);

        assertThat(replacedSystemProperties.size(), is(0));

        Map.Entry entry = getFirstEntry(propertyContainerWithNullValue);

        assertThat(entry.getValue(), nullValue());
    }

    @Test
    public void getPrefixedSystemProperties() {
        clearSystemProperties();

        {
            PropertyContainer currentPropertiesWithPrefix = getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(currentPropertiesWithPrefix.size(), is(0));
        }

        String key = KNOWN_KEY_2;
        String value = KNOWN_VALUE_2;

        System.setProperty(key, value);

        {
            PropertyContainer currentPropertiesWithPrefix = getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(currentPropertiesWithPrefix.size(), is(1));

            Map.Entry<String, String> entry = getFirstEntry(currentPropertiesWithPrefix);

            assertThat(entry.getKey(), is(key));
            assertThat(entry.getValue(), is(value));
        }
    }

    private void clearSystemProperties() {
        System.clearProperty(KNOWN_KEY_1);
        System.clearProperty(KNOWN_KEY_2);
    }

    private Map.Entry<String, String> getFirstEntry(PropertyContainer replacedSystemProperties) {
        Set<Map.Entry<String, String>> entries = replacedSystemProperties.entrySet();

        return (Map.Entry<String, String>) entries.toArray()[0];
    }
}
