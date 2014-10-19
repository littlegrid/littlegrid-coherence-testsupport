package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.TabularResultSet;

import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Management service implementation.
 */
class DefaultManagementService implements ManagementService {
    private static final Logger LOGGER = Logger.getLogger(DefaultManagementService.class.getName());

    private final ManagementRepository managementRepository;
    private final Properties aliases;
    private final String aliasExpansionIndicator;
    private final String aliasValueDelimiter;

    /**
     * Constructor.
     *
     * @param managementRepository Management repository.
     * @param aliasValueDelimiter
     */
    public DefaultManagementService(final ManagementRepository managementRepository,
                                    final Properties aliases,
                                    final String aliasExpansionIndicator,
                                    final String aliasValueDelimiter) {

        this.managementRepository = managementRepository;
        this.aliases = aliases;
        this.aliasExpansionIndicator = aliasExpansionIndicator;
        this.aliasValueDelimiter = aliasValueDelimiter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getAliasPrefix() {
        return aliasExpansionIndicator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findManagementInformation(final String query) {
        final String queryToExecute;

        //TODO: hack something to test concept
        if (query.contains(aliasExpansionIndicator)) {
            LOGGER.info("Alias expansion to be performed");

            final int startOfAlias = query.indexOf(aliasExpansionIndicator);
            final int endOfAlias = query.indexOf(":", startOfAlias);

            final String alias;
            final String valuesToExpand;

            if (endOfAlias == -1) {
                alias = query.substring(startOfAlias);
                valuesToExpand = null;
            } else {
                alias = query.substring(startOfAlias, endOfAlias);

                final int endOfValuesToExpand = query.indexOf(" ", endOfAlias + 1);

                System.out.println("E: " + endOfValuesToExpand);
                if (endOfValuesToExpand == -1) {
                    valuesToExpand = query.substring(endOfAlias + 1);
                } else {
                    valuesToExpand = query.substring(endOfAlias + 1, endOfValuesToExpand);
                }
            }

            LOGGER.info(format("Alias identified as: %s", alias));

            if (aliases.containsKey(alias)) {
                final String queryToExpand = aliases.getProperty(alias);

                if (valuesToExpand == null) {
                    LOGGER.info(format("No expansion required for: %s", queryToExpand));

                    queryToExecute = queryToExpand;
                } else {
                    System.out.println("Values to expand: " + valuesToExpand);
/*
                    LOGGER.info(format("Query to expand: %, value expansion string: %s",
                            queryToExpand, valuesToExpand));
*/

                    final String[] valuesToUse = valuesToExpand.split(aliasValueDelimiter);

                    LOGGER.info(format("About to expand: %s, using: %s", queryToExpand, Arrays.deepToString(valuesToUse)));

                    String expandedQuery = queryToExpand;
                    int counter = 1;
                    for (final String valueToUse : valuesToUse) {
                        expandedQuery = expandedQuery.replaceAll("%" + counter, valueToUse);

                        counter++;
                    }

                    LOGGER.info(format("Expanded query: %s", expandedQuery));

                    queryToExecute = expandedQuery;
                }
            } else {
                throw new UnsupportedOperationException("Alias not recognised");
            }
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
    public TabularResultSet createManagementInformationSnapshot(final String snapshotName,
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
