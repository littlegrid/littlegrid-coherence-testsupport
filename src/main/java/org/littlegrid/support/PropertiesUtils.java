package org.littlegrid.support;

import com.tangosol.util.Resources;

import java.net.URL;
import java.util.Properties;

import static java.lang.String.format;

/**
 * Properties utilities class, containing useful convenience methods for working with properties.
 */
public final class PropertiesUtils {
    private static final LoggerPlaceHolder LOGGER = new LoggerPlaceHolder(PropertiesUtils.class.getName());

    private static final String DELIMITER = ",";

    /**
     * Private constructor to prevent creation.
     */
    private PropertiesUtils() {
    }

    /**
     * Loads properties using the specified (potentially comma delimited) filenames.
     *
     * @param commaDelimitedPropertiesFilenames
     *         Comma delimited properties filenames.
     * @return properties.
     */
    public static Properties loadProperties(final String commaDelimitedPropertiesFilenames) {
        return loadProperties(commaDelimitedPropertiesFilenames.split(DELIMITER));
    }

    /**
     * Loads properties using the specified array of string properties filenames.
     *
     * @param propertiesFilenames Properties filenames.
     * @return properties.
     */
    public static Properties loadProperties(final String... propertiesFilenames) {
        final Properties properties = new Properties();

        for (String propertiesFilename : propertiesFilenames) {
            propertiesFilename = propertiesFilename.trim();

            final URL url = Resources.findFileOrResource(propertiesFilename,
                    PropertiesUtils.class.getClass().getClassLoader());

            if (url == null) {
                LOGGER.info(format("File '%s' not found - no properties loaded", propertiesFilename));

                continue;
            }

            try {
                final Properties currentProperties = new Properties();
                currentProperties.load(url.openStream());

                LOGGER.info(format("File '%s' found and '%s' properties loaded", propertiesFilename,
                        currentProperties.size()));

                properties.putAll(currentProperties);
            } catch (Exception e) {
                throw new IllegalArgumentException(format(
                        "Cannot load properties file: '%s' due to: %s", propertiesFilename, e));
            }
        }

        return properties;
    }
}
