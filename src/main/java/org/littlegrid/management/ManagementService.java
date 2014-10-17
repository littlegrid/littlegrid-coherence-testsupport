package org.littlegrid.management;

import javax.management.MBeanServerConnection;
import java.util.Collection;
import java.util.Properties;

/**
 * Management service.
 */
public interface ManagementService {
    String getAliasExpansionIndicator();

    /**
     * Finds management information based upon the supplied query.
     *
     * @param query Query.
     * @return results.
     */
    TabularResultSet findManagementInformation(String query);

    int createManagementInformationSnapshot(String snapshotName,
                                            String snapshotQuery);

    boolean dropManagementInformationSnapshot(String snapshotName);

    TabularResultSet findSnapshots();

    TabularResultSet describeSnapshot(String snapshotName);

    Object updateManagementInformation(Object notSureYet);

    Object executeManagementInformationFunction(Object notSureYet);


    interface Builder {
        String BUILDER_SYSTEM_PROPERTY_PREFIX_KEY = "littlegrid.management.builder.";

        String BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY = "littlegrid_management_builder_";

        Builder setAliases(String commaDelimitedPropertiesFilenames);

        Builder setUrlPath(String urlPath);

        Builder setUsername(String username);

        Builder setPassword(String password);

        Builder setMBeanServerConnection(MBeanServerConnection mBeanServerConnection);

        Builder setAliasExpansionIndicator(String aliasExpansionIndicator);

        Builder setAliasValueDelimiter(String aliasValueDelimiter);

        ManagementService buildAndConnect();
    }
}
