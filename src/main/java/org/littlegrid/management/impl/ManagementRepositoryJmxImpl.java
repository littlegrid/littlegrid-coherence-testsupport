package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import static com.tangosol.util.InvocableMap.EntryAggregator;
import static java.lang.String.format;

/**
 * Management repository JMX implementation.
 */
class ManagementRepositoryJmxImpl implements ManagementRepository {
    private static final Logger LOGGER = Logger.getLogger(ManagementRepositoryJmxImpl.class.getName());

    private final MBeanServerConnection mBeanServerConnection;

    //TODO: think this through a bit more
    private final Map<String, TabularResultSet> snapshots = new HashMap<String, TabularResultSet>();

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

//        final Set<Map.Entry> results2 = PostQueryResultUtils.performPostRestriction(results, restriction);

//        final Collection<Map<String, Object>> results3 = PostQueryResultUtils.performPostProjection(results2, projection);
//        System.out.println("After restriction: " + results2);
        /*
            Perform the query

            Determine the MBean type returned

            Populate the model

            Perform the post restriction and projection

            Convert the results from the model into the generic return type.
         */
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

        snapshots.put(snapshotName, results);

        return results.getRowCount() * results.getRowCount();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean dropManagementInformationSnapshot(final String snapshotName) {
        final TabularResultSet results = snapshots.remove(snapshotName);

        return results != null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<String> findSnapshots() {
        return new HashSet<String>(snapshots.keySet());
    }

    @SuppressWarnings("unchecked")
    private TabularResultSet performQuery(final String queryTarget) {
        final TabularResultSet cachedResults = snapshots.get(queryTarget);

        if (cachedResults != null) {
            LOGGER.info("Found snapshot for re-use");

            return cachedResults;
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

                //TODO: change this to getAttributes
                for (final MBeanAttributeInfo info : attributes) {
                    final String key = info.getName();

                    row.put(key, mBeanServerConnection.getAttribute(objectName, key));
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
}
