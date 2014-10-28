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
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;

/**
 * Management repository JMX implementation.
 *
 * @since 2.16
 */
class DefaultManagementRepository implements ManagementRepository {
    private static final Logger LOGGER = Logger.getLogger(DefaultManagementRepository.class.getName());

    private final MBeanServerConnection mBeanServerConnection;
    private final Map<String, Snapshot> snapshots = new LinkedHashMap<String, Snapshot>();
    private final String aliasPrefix;
    private final String snapshotPrefix;

    /**
     * Constructor.
     *
     * @param mBeanServerConnection MBean server connection.
     * @param aliasPrefix           Alias prefix character(s).
     * @param snapshotPrefix        Snapshot prefix character(s).
     */
    public DefaultManagementRepository(final MBeanServerConnection mBeanServerConnection,
                                       final String aliasPrefix,
                                       final String snapshotPrefix) {

        this.mBeanServerConnection = mBeanServerConnection;
        this.aliasPrefix = aliasPrefix;
        this.snapshotPrefix = snapshotPrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findManagementInformationByCriteria(final ValueExtractor projection,
                                                                final String queryTarget,
                                                                final Filter restriction) {

        final TabularResultSet queryResults = performQuery(queryTarget);

        return new DefaultQueryPostProcessorProjection(queryResults, projection, restriction).getResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findManagementInformationByCriteria(final EntryAggregator aggregation,
                                                                final String queryTarget,
                                                                final Filter restriction) {

        final TabularResultSet queryResults = performQuery(queryTarget);

        return new DefaultQueryPostProcessorAggregation(queryResults, aggregation, restriction).getResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet createManagementInformationSnapshot(final String snapshotName,
                                                                final String snapshotQuery) {

        LOGGER.info(format("About to create snapshot '%s' using '%s' query", snapshotName, snapshotQuery));

        final TabularResultSet results = performQuery(snapshotQuery);

        final String createdSnapshotName;

        if (snapshotName.startsWith(snapshotPrefix)) {
            createdSnapshotName = snapshotName;
        } else {
            createdSnapshotName = snapshotPrefix + snapshotName;
        }

        snapshots.put(createdSnapshotName, new Snapshot(snapshotQuery, results));

        final TabularResultSet snapshotDetails = new DefaultTabularResultSet();
        snapshotDetails.addRow(Collections.<String, Object>singletonMap("Name", createdSnapshotName));
        snapshotDetails.addRow(Collections.<String, Object>singletonMap("Rows", results.getRowCount()));
        snapshotDetails.addRow(Collections.<String, Object>singletonMap("Columns", results.getColumnCount()));

        return snapshotDetails;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean dropManagementInformationSnapshot(final String snapshotName) {
        final Snapshot snapshot = snapshots.remove(snapshotName);

        return snapshot != null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet findSnapshots() {
        final TabularResultSet summary = new DefaultTabularResultSet();

        for (final Map.Entry<String, Snapshot> entry : snapshots.entrySet()) {
            final Map<String, Object> row = new LinkedHashMap<String, Object>();
            row.put("Name", entry.getKey());

            final Snapshot snapshot = entry.getValue();

            row.put("Query", snapshot.getQuery());
            row.put("Created", snapshot.getCreatedDate());

            final TabularResultSet results = snapshot.getResults();

            row.put("Rows", results.getRowCount());
            row.put("Column", results.getColumnCount());

            summary.addRow(row);
        }

        return summary;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TabularResultSet describeSnapshot(final String snapshotName) {
        final TabularResultSet results = new DefaultTabularResultSet();
        final Snapshot snapshot = snapshots.get(snapshotName);
        final Set<String> sortedColumnNames = new TreeSet<String>();

        if (snapshot != null) {
            //TODO: for now sort the columns
            for (String columnName : snapshot.getResults().getColumnNames()) {
                sortedColumnNames.add(columnName);
            }

            for (String columnName : sortedColumnNames) {
                results.addRow(Collections.<String, Object>singletonMap(columnName, "TODO"));
            }
        }

        return results;
    }

    @SuppressWarnings("unchecked")
    private TabularResultSet performQuery(final String queryTarget) {
        final Snapshot snapshot = snapshots.get(queryTarget);

        if (snapshot != null) {
            LOGGER.info("Found snapshot for re-use");

            return snapshot.getResults();
        }

        LOGGER.info(format("About to perform query using: %s", queryTarget));

        try {
            final long startTime = System.currentTimeMillis();
            final Set<ObjectName> queryResults =
                    mBeanServerConnection.queryNames(new ObjectName(queryTarget), null);

            final TabularResultSet results = new DefaultTabularResultSet();
            String[] attributeKeys = null;

            for (final ObjectName objectName : queryResults) {
                final Map<String, Object> row = new HashMap<String, Object>();

                if (attributeKeys == null) {
                    final MBeanAttributeInfo[] attributes =
                            mBeanServerConnection.getMBeanInfo(objectName).getAttributes();

                    final Collection<String> temporaryKeys = new ArrayList<String>();

                    for (final MBeanAttributeInfo info : attributes) {
                        final String key = info.getName();

                        temporaryKeys.add(key);
                    }

                    attributeKeys = temporaryKeys.toArray(new String[temporaryKeys.size()]);
                }

                final AttributeList attributes = mBeanServerConnection.getAttributes(objectName, attributeKeys);

                for (final Attribute attribute : attributes.asList()) {
                    //TODO: potentially re-use key strings
                    row.put(attribute.getName(), attribute.getValue());
                }

                for (final Map.Entry entry : objectName.getKeyPropertyList().entrySet()) {
                    //TODO: potentially re-use key strings
                    row.put(entry.getKey().toString(), entry.getValue());
                }

                results.addRow(row);
            }

            final long duration = System.currentTimeMillis() - startTime;
            final int rowCount = results.getRowCount();
            final int columnCount = results.getColumnCount();
            final int totalAttributeValues = rowCount * columnCount;

            LOGGER.info(format("Completed query in %dms, resulting in %d rows and %d columns (%d attribute values)",
                    duration, rowCount, columnCount, totalAttributeValues));

            return results;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static class Snapshot {
        private final Date createdDate = new Date();
        private final String query;
        private final TabularResultSet results;

        private Snapshot(final String query,
                         final TabularResultSet results) {

            this.query = query;
            this.results = results;
        }

        public Date getCreatedDate() {
            return createdDate;
        }

        public String getQuery() {
            return query;
        }

        public TabularResultSet getResults() {
            return results;
        }
    }
}
