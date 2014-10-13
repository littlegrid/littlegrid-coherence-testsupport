package org.littlegrid.management.impl;

import com.tangosol.util.Filter;
import com.tangosol.util.ValueExtractor;
import org.littlegrid.management.TabularResultSet;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static com.tangosol.util.InvocableMap.EntryAggregator;

/**
 * Management repository JMX implementation.
 */
public class ManagementRepositoryJmxImpl implements ManagementRepository {
    private final MBeanServerConnection mBeanServerConnection;

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

    @SuppressWarnings("unchecked")
    private TabularResultSet performQuery(final String queryTarget) {
        try {
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

            return results;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
