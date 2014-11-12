/*
 * Copyright (c) 2010-2014 Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the littlegrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid.management.impl;

import org.littlegrid.impl.Info;
import org.littlegrid.management.ManagementService;
import org.littlegrid.support.BeanUtils;
import org.littlegrid.support.PropertiesUtils;
import org.littlegrid.support.StringUtils;
import org.littlegrid.support.SystemUtils;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import java.util.HashMap;
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
    private String aliasPrefix;
    private String snapshotPrefix;
    private String aliasValueDelimiter;

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
        aliases = PropertiesUtils.loadProperties(Level.INFO,
                "littlegrid/littlegrid-management-alias-default.properties");
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
        this.username = username;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setPassword(final String password) {
        this.password = password;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAliasPrefix(final String aliasPrefix) {
        this.aliasPrefix = aliasPrefix;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setSnapshotPrefix(final String snapshotPrefix) {
        this.snapshotPrefix = snapshotPrefix;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setAliasValueDelimiter(final String aliasValueDelimiter) {
        this.aliasValueDelimiter = aliasValueDelimiter;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ManagementService build() {
        //TODO: add connecting to log message
        try {
            final Map<String, Object> env = new HashMap<String, Object>();

            if (StringUtils.stringHasValue(username)) {
                final String[] credentials = {username, password};

                env.put(JMXConnector.CREDENTIALS, credentials);
            }

            //TODO: Add check to throw illegal state exception or something like that if null...
            final JMXServiceURL jmxUrl = new JMXServiceURL(urlPath);
            final JMXConnector jmxConnector = JMXConnectorFactory.connect(jmxUrl, env);

            return build(jmxConnector.getMBeanServerConnection());
        } catch (Exception e) {
            throw new RuntimeException(format("Exception connecting to MBean server: %s", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ManagementService build(final MBeanServerConnection mBeanServerConnection) {
        final ManagementRepository managementRepository = new DefaultManagementRepository(
                mBeanServerConnection, aliasPrefix, snapshotPrefix);

        return new DefaultManagementService(managementRepository, aliases,
                aliasPrefix, aliasValueDelimiter, snapshotPrefix);
    }
}
