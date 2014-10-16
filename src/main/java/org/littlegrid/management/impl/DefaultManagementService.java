package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.TabularResultSet;

import java.util.Collection;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Management service implementation.
 */
class DefaultManagementService implements ManagementService {
    private static final Logger LOGGER = Logger.getLogger(DefaultManagementService.class.getName());

    private final ManagementRepository managementRepository;
    private final Properties aliases;

    /**
     * Constructor.
     *
     * @param managementRepository  Management repository.
     */
    public DefaultManagementService(final ManagementRepository managementRepository,
                                    final Properties aliases) {

        this.managementRepository = managementRepository;
        this.aliases = aliases;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findManagementInformation(final String query) {
        final String queryToExecute;

        if (aliases.containsKey(query)) {
            queryToExecute = aliases.getProperty(query);
        } else {
            queryToExecute = query;
        }

        final QueryParser parser = new DefaultQueryParser(queryToExecute);
        final String queryTarget = parser.getTarget();
        final Filter restriction = parser.getRestriction();

        if (parser.isAggregation()) {
            return managementRepository.findManagementInformationByCriteria(
                    parser.getAggregation(), queryTarget, restriction);
        } else {
            return managementRepository.findManagementInformationByCriteria(
                    parser.getProjection(), queryTarget, restriction);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int createManagementInformationSnapshot(final String snapshotName,
                                                   final String snapshotQuery) {

        LOGGER.info("Experimental feature: " + snapshotName + ", " + snapshotQuery);

        final String queryToExecute;

        if (aliases.containsKey(snapshotQuery)) {
            queryToExecute = aliases.getProperty(snapshotQuery);
        } else {
            queryToExecute = "select value() from " + snapshotQuery;
        }

        final QueryParser parser = new DefaultQueryParser(queryToExecute);

        return managementRepository.createManagementInformationSnapshot(snapshotName, parser.getTarget());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean dropManagementInformationSnapshot(final String snapshotName) {
        LOGGER.info("Experimental feature");

        return managementRepository.dropManagementInformationSnapshot(snapshotName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findSnapshots() {
        LOGGER.info("Experimental feature");

        return managementRepository.findSnapshots();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet describeSnapshot(final String snapshotName) {
        return managementRepository.describeSnapshot(snapshotName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object updateManagementInformation(final Object notSureYet) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object executeManagementInformationFunction(final Object notSureYet) {
        throw new UnsupportedOperationException();
    }
}
