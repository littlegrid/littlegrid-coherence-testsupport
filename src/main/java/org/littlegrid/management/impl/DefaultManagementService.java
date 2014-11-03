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

import com.tangosol.util.Filter;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.TabularResult;

import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Management service defult implementation.
 *
 * @since 2.16
 */
class DefaultManagementService implements ManagementService {
    private static final Logger LOGGER = Logger.getLogger(DefaultManagementService.class.getName());

    private final ManagementRepository managementRepository;
    private final Properties aliases;
    private final String aliasPrefix;
    private final String aliasValueDelimiter;
    private final String snapshotPrefix;

    /**
     * Constructor.
     *
     * @param managementRepository Management repository.
     * @param aliases              Aliases,
     * @param aliasPrefix          Alias prefix.
     * @param aliasValueDelimiter  Delimiter between alias values.
     */
    public DefaultManagementService(final ManagementRepository managementRepository,
                                    final Properties aliases,
                                    final String aliasPrefix,
                                    final String aliasValueDelimiter,
                                    final String snapshotPrefix) {

        this.managementRepository = managementRepository;
        this.aliases = aliases;
        this.aliasPrefix = aliasPrefix;
        this.aliasValueDelimiter = aliasValueDelimiter;
        this.snapshotPrefix = snapshotPrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getAliasPrefix() {
        return aliasPrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResult findManagementInformation(final String query) {
        final String queryToExecute;

        //TODO: hack something to test concept
        if (query.contains(aliasPrefix)) {
            LOGGER.info("Alias expansion to be performed");

            final int startOfAlias = query.indexOf(aliasPrefix);
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
                throw new UnsupportedOperationException(format("Alias '%s' not recognised", alias));
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
    public TabularResult createSnapshot(final String snapshotName,
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
    public boolean dropSnapshot(final String snapshotName) {
        LOGGER.info("Experimental feature");

        return managementRepository.dropManagementInformationSnapshot(snapshotName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResult findSnapshots() {
        LOGGER.info("Experimental feature");

        return managementRepository.findSnapshots();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResult describeSnapshot(final String snapshotName) {
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
