package org.littlegrid.management.impl;

import org.littlegrid.impl.Info;
import org.littlegrid.management.ManagementService;
import org.littlegrid.support.BeanUtils;
import org.littlegrid.support.PropertiesUtils;
import org.littlegrid.support.SystemUtils;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.management.ManagementService.Builder;

/**
 * Default management service builder.
 *
 * @since 2.16
 */
public class DefaultManagementServiceBuilder implements Builder {
    private static final String LITTLEGRID_DIRECTORY_SLASH = "littlegrid/";

    private static final String DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-management-builder-default.properties";

    private static final String OVERRIDE_PROPERTIES_FILENAME = "littlegrid-management-builder-override.properties";

    private static final Logger LOGGER = Logger.getLogger(DefaultManagementServiceBuilder.class.getName());

    private String urlPath;
    private String username;
    private String password;
    private MBeanServerConnection mBeanServerConnection;
    private Properties aliases;

    /**
     * Constructor.
     */
    public DefaultManagementServiceBuilder() {
        final Map<String, Integer> builderKeysAndValuesLoadedSummary = new LinkedHashMap<String, Integer>();

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary);

        LOGGER.info(format("___ %s %s (%s) - initialised.  Management builder values: %s.  ",
                Info.getName(), Info.getVersionNumber(), Info.getWebsiteAddress(),
                builderKeysAndValuesLoadedSummary));

        //TODO: still all experimental stuff
        aliases = PropertiesUtils.loadProperties(Level.INFO, "littlegrid/littlegrid-management-alias-default.properties");
    }

    private void loadAndSetBuilderKeysAndValues(final Map<String, Integer> builderKeysAndValuesLoadedSummary) {
        loadAndSetBuilderKeysAndValuesUsingPropertiesFiles(builderKeysAndValuesLoadedSummary);

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "environment variables",
                BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY, SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "environment variables",
                BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY.toUpperCase(), SystemUtils.getEnvironmentVariables());

        loadAndSetBuilderKeysAndValues(builderKeysAndValuesLoadedSummary, "system properties",
                BUILDER_SYSTEM_PROPERTY_PREFIX_KEY, System.getProperties());
    }

    private void loadAndSetBuilderKeysAndValues(final Map<String, Integer> builderKeysAndValuesLoadedSummary,
                                                final String propertiesDescription,
                                                final String prefix,
                                                final Properties environmentVariablesOrSystemProperties) {

        final Properties builderOverrides = SystemUtils.getPropertiesWithPrefix(
                environmentVariablesOrSystemProperties, prefix, true);

        LOGGER.fine(format("Prefixed '%s' %s found: %d", prefix, propertiesDescription, builderOverrides.size()));
        builderKeysAndValuesLoadedSummary.put(format("'%s' %s", prefix, propertiesDescription),
                builderOverrides.size());

        BeanUtils.multiSetter(this, builderOverrides);
    }

    private void loadAndSetBuilderKeysAndValuesUsingPropertiesFiles(
            final Map<String, Integer> builderKeysAndValuesLoadedSummary) {

        final Properties defaultProperties = PropertiesUtils.loadProperties(Level.FINE, DEFAULT_PROPERTIES_FILENAME);

        BeanUtils.multiSetter(this, defaultProperties);
        builderKeysAndValuesLoadedSummary.put("default file", defaultProperties.size());

        final Properties overrideProperties = PropertiesUtils.loadProperties(Level.FINE,
                OVERRIDE_PROPERTIES_FILENAME,
                LITTLEGRID_DIRECTORY_SLASH + OVERRIDE_PROPERTIES_FILENAME);

        BeanUtils.multiSetter(this, overrideProperties);
        builderKeysAndValuesLoadedSummary.put("override file", overrideProperties.size());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAliases(String commaDelimitedPropertiesFilenames) {
        final Properties properties =
                PropertiesUtils.loadProperties(Level.INFO, commaDelimitedPropertiesFilenames);

        BeanUtils.multiSetter(this, properties);

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setUrlPath(final String urlPath) {
        this.urlPath = urlPath;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setUsername(final String username) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setPassword(final String password) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setMBeanServerConnection(final MBeanServerConnection mBeanServerConnection) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ManagementService buildAndConnect() {
        try {
            final JMXServiceURL jmxUrl = new JMXServiceURL(urlPath);
            final JMXConnector jmxConnector = JMXConnectorFactory.connect(jmxUrl, null);
            final MBeanServerConnection mBeanServer = jmxConnector.getMBeanServerConnection();
            final ManagementRepository managementRepository = new ManagementRepositoryJmxImpl(mBeanServer);

            return new DefaultManagementService(managementRepository, aliases);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
