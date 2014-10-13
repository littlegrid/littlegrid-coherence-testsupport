package org.littlegrid.management;

import org.littlegrid.management.impl.DefaultManagementServiceBuilder;

import static org.littlegrid.management.ManagementService.Builder;

/**
 * Management utilities.
 */
public class ManagementUtils {
    private ManagementUtils() {
    }

    public static Builder newManagementBuilder() {
        return new DefaultManagementServiceBuilder();
    }

    public void shutdownManagementServices(final ManagementService... managementServices) {
        throw new UnsupportedOperationException();
    }
}
