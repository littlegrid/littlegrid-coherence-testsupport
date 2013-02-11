package org.littlegrid.support;

import com.tangosol.util.Resources;

import java.net.URL;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Properties utilities class, containing useful convenience methods for working with properties.
 */
public final class PropertiesUtils {
    private static final Logger LOGGER = Logger.getLogger(PropertiesUtils.class.getName());

    private static final String DELIMITER = ",";

    /**
     * Default scope to enable test coverage.
     */
    PropertiesUtils() {
        throw new UnsupportedOperationException();
    }

    /**
     * Loads properties using the specified (potentially comma delimited) filenames.
     *
     * @param loadedPropertyFileLogLevel Log level at which properties files loaded should be output.
     * @param commaDelimitedPropertiesFilenames
     *         Comma delimited properties filenames.
     * @return properties.
     */
    public static Properties loadProperties(final Level loadedPropertyFileLogLevel,
                                            final String commaDelimitedPropertiesFilenames) {

        return loadProperties(loadedPropertyFileLogLevel, commaDelimitedPropertiesFilenames.split(DELIMITER));
    }

    /**
     * Loads properties using the specified array of string properties filenames.
     *
     * @param loadedPropertyFileLogLevel Log level at which properties files loaded should be output.
     * @param propertiesFilenames        Properties filenames.
     * @return properties.
     */
    public static Properties loadProperties(final Level loadedPropertyFileLogLevel,
                                            final String... propertiesFilenames) {

        final StringBuilder sb = new StringBuilder();
        final Properties properties = new Properties();

        for (String propertiesFilename : propertiesFilenames) {
            propertiesFilename = propertiesFilename.trim();

            final URL url = Resources.findFileOrResource(propertiesFilename,
                    PropertiesUtils.class.getClass().getClassLoader());

            if (url == null) {
                sb.append(" '");
                sb.append(propertiesFilename);
                sb.append("'");

                continue;
            }

            try {
                final Properties currentProperties = new Properties();
                currentProperties.load(url.openStream());

                LOGGER.log(loadedPropertyFileLogLevel,
                        format("File '%s' found and %d properties loaded", propertiesFilename,
                                currentProperties.size()));

                properties.putAll(currentProperties);
            } catch (Exception e) {
                throw new IllegalArgumentException(format(
                        "Cannot load properties file: '%s' due to: %s", propertiesFilename, e));
            }
        }

        if (sb.length() > 0) {
            LOGGER.info("Properties file(s) not found, no properties loaded:" + sb);
        }

        return properties;
    }
}
