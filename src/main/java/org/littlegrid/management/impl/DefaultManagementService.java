package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.TabularResultSet;

/**
 * Management service implementation.
 */
public class DefaultManagementService implements ManagementService {
    private final ManagementRepository managementRepository;

    public DefaultManagementService(final ManagementRepository managementRepository) {
        this.managementRepository = managementRepository;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findManagementInformation(final String query) {
        final QueryParser parser = new DefaultQueryParser(query);
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
