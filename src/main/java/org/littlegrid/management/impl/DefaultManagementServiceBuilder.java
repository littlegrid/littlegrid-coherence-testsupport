package org.littlegrid.management.impl;

import org.littlegrid.management.ManagementService;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import static org.littlegrid.management.ManagementService.Builder;

/**
 * Default management service builder.
 */
public class DefaultManagementServiceBuilder implements Builder {
    private String urlPath;
    private String username;
    private String password;
    private MBeanServerConnection mBeanServerConnection;

    private static final String DEFAULT_PROPERTIES_FILENAME =
            "littlegrid/littlegrid-management-builder-default.properties";

    private static final String OVERRIDE_PROPERTIES_FILENAME = "littlegrid-management-builder-override.properties";


    public DefaultManagementServiceBuilder() {
        this.urlPath = "/jndi/rmi://localhost:50002/jmxrmi";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Builder setUrlPath(final String urlPath) {
        throw new UnsupportedOperationException();
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
            final JMXServiceURL jmxUrl = new JMXServiceURL("rmi", "", 0, urlPath);
            final JMXConnector jmxConnector = JMXConnectorFactory.connect(jmxUrl, null);
            final MBeanServerConnection mBeanServer = jmxConnector.getMBeanServerConnection();
            final ManagementRepository managementRepository = new ManagementRepositoryJmxImpl(mBeanServer);

            return new DefaultManagementService(managementRepository);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
