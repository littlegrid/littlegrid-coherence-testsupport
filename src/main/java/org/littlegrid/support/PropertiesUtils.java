package org.littlegrid.support;

import com.tangosol.util.Resources;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

import java.net.URL;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

import static java.lang.String.format;

/**
 * Properties utilities class, containing useful convenience methods for working with properties.
 */
public final class PropertiesUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(PropertiesUtils.class);

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
     * @param loadedPropertyFileLogLevel        Log level at which properties files loaded should be output.
     * @param commaDelimitedPropertiesFilenames Comma delimited properties filenames.
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

        final Map<String, String> loadedSummary = new LinkedHashMap<>();
        final Properties properties = new Properties();

        for (String propertiesFilename : propertiesFilenames) {
            propertiesFilename = propertiesFilename.trim();

            final URL url = Resources.findFileOrResource(propertiesFilename,
                    PropertiesUtils.class.getClassLoader());

            if (url == null) {
                loadedSummary.put(propertiesFilename, "not found");

                continue;
            }

            try {
                final Properties currentProperties = new Properties();
                currentProperties.load(url.openStream());

                properties.putAll(currentProperties);
                loadedSummary.put(propertiesFilename, Integer.toString(currentProperties.size()));
            } catch (Exception e) {
                throw new IllegalArgumentException(format(
                        "Cannot load properties file: '%s' due to: %s", propertiesFilename, e));
            }
        }

        if (loadedPropertyFileLogLevel == Level.DEBUG) {
            LOGGER.debug(loadedSummary.toString());
        } else {
            LOGGER.info(loadedSummary.toString());
        }

        return properties;
    }
}
