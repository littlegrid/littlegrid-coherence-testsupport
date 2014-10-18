package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;
import static java.util.Collections.singletonMap;

/**
 * Management repository JMX implementation.
 */
class ManagementRepositoryJmxImpl implements ManagementRepository {
    private static final Logger LOGGER = Logger.getLogger(ManagementRepositoryJmxImpl.class.getName());

    private final MBeanServerConnection mBeanServerConnection;

    //TODO: think this through a bit more
    private final Map<String, Snapshot> snapshots = new LinkedHashMap<String, Snapshot>();

    /**
     * Constructor.
     *
     * @param mBeanServerConnection MBean server connection.
     */
    public ManagementRepositoryJmxImpl(final MBeanServerConnection mBeanServerConnection) {
        this.mBeanServerConnection = mBeanServerConnection;
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
    public int createManagementInformationSnapshot(final String snapshotName,
                                                   final String snapshotQuery) {

        LOGGER.info(format("About to create snapshot '%s' using '%s' query", snapshotName, snapshotQuery));

        final TabularResultSet results = performQuery(snapshotQuery);

        snapshots.put(snapshotName, new Snapshot(snapshotQuery, results));

        //TODO: check why return seems different from output
        return results.getRowCount() * results.getRowCount();
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

        //TODO: check for null
        for (String columnName : snapshot.getResults().getColumnNames()) {
            results.addRow(Collections.<String, Object>singletonMap(columnName, "TODO"));
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
            final Set<ObjectInstance> queryResults =
                    mBeanServerConnection.queryMBeans(new ObjectName(queryTarget), null);

            final TabularResultSet results = new DefaultTabularResultSet();

            for (final ObjectInstance objectInstance : queryResults) {
                final ObjectName objectName = objectInstance.getObjectName();
                final Map<String, Object> row = new HashMap<String, Object>();
                final MBeanAttributeInfo[] attributes = mBeanServerConnection.getMBeanInfo(objectName).getAttributes();

                final Collection<String> keys = new ArrayList<String>();

                //TODO: change this to getAttributes
                for (final MBeanAttributeInfo info : attributes) {
                    final String key = info.getName();

                    keys.add(key);
                }

                final AttributeList attributeList = mBeanServerConnection.getAttributes(objectName,
                        keys.toArray(new String[keys.size()]));

                for (final Object object : attributeList) {
                    final Attribute attribute = (Attribute) object;

                    row.put(attribute.getName(), attribute.getValue());
                }

                for (final Map.Entry entry : objectName.getKeyPropertyList().entrySet()) {
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
