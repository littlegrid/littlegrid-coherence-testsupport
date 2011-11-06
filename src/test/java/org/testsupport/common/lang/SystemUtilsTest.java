package org.testsupport.common.lang;

import org.junit.Before;
import org.junit.Test;
import org.testsupport.common.AbstractTest;

import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

/**
 * System utils tests.
 */
public class SystemUtilsTest extends AbstractTest {
    private static final String KNOWN_PREFIX = "prefix.used.for.testing.";
    private static final String KEY1_WITH_KNOWN_PREFIX = KNOWN_PREFIX + "key1";
    private static final String KNOWN_VALUE_1 = "value1";
    private static final String KEY2_WITH_KNOWN_PREFIX = KNOWN_PREFIX + "key2";
    private static final String KNOWN_VALUE_2 = "value2";

    @Before
    public void clearSystemProperties() {
        System.clearProperty(KEY1_WITH_KNOWN_PREFIX);
        System.clearProperty(KEY2_WITH_KNOWN_PREFIX);
    }

    @Test
    public void getPrefixedSystemProperties() {
        clearSystemProperties();

        {
            Properties systemPropertiesWithPrefix = SystemUtils.getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(systemPropertiesWithPrefix.size(), is(0));
        }

        String key = KEY2_WITH_KNOWN_PREFIX;
        String value = KNOWN_VALUE_2;

        System.setProperty(key, value);

        {
            Properties systemPropertiesWithPrefix = SystemUtils.getSystemPropertiesWithPrefix(KNOWN_PREFIX);
            assertThat(systemPropertiesWithPrefix.size(), is(1));

            Map.Entry<String, String> entry = getFirstEntry(systemPropertiesWithPrefix);

            assertThat(entry.getKey(), is(key));
            assertThat(entry.getValue(), is(value));
        }
    }

    @Test
    public void snapshotSystemProperties() {
        Properties propertiesBeforeChange = SystemUtils.snapshotSystemProperties();

        final int beforeChangeSize = propertiesBeforeChange.size();

        System.setProperty(KEY1_WITH_KNOWN_PREFIX, KNOWN_VALUE_1);

        assertThat(System.getProperties().size(), not(beforeChangeSize));
        assertThat(System.getProperties(), not(propertiesBeforeChange));
    }

    @Test
    public void applyToSystemProperties() {
        final String key = KEY1_WITH_KNOWN_PREFIX;
        final String value = KNOWN_VALUE_1;

        Properties properties = new Properties();
        properties.setProperty(key, value);

        SystemUtils.applyToSystemProperties(properties);

        assertThat(System.getProperties().containsKey(key), is(true));
        assertThat(System.getProperty(key), is(KNOWN_VALUE_1));
    }

    @SuppressWarnings("unchecked")
    private static Map.Entry<String, String> getFirstEntry(final Properties properties) {
        Set<Map.Entry<Object, Object>> entries = properties.entrySet();

        return (Map.Entry<String, String>) entries.toArray()[0];
    }
}
