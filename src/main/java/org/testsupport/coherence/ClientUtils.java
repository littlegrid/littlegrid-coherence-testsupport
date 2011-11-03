package org.testsupport.coherence;

import org.testsupport.coherence.impl.DefaultClusterMemberGroupBuilder;
import org.testsupport.common.lang.PropertyContainer;
import org.testsupport.common.lang.SystemUtils;

import java.util.Properties;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Coherence client utils.
 */
@Deprecated
public class ClientUtils {
    private static final Logger LOGGER = Logger.getLogger(ClientUtils.class.getName());

    /**
     * Private constructor to prevent creation.
     */
    private ClientUtils() {
    }

    /**
     * Sets the system properties for a storage-disabled client.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties() {
        setStorageDisabledClientSystemProperties(null, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties(String cacheConfiguration) {
        setStorageDisabledClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
    @Deprecated
    public static void setStorageDisabledClientSystemProperties(String cacheConfiguration,
                                                                Properties properties) {

        PropertyContainer containerToUse = internalCreateStorageDisabledClientPropertyContainerWithDefaults();
        containerToUse.addProperties(properties);

        internalSetGenericClientSystemProperties(cacheConfiguration, containerToUse);
    }

    /**
     * Sets the system properties for an Extend client.
     *
     * @param cacheConfiguration Cache configuration.
     */
    @Deprecated
    public static void setExtendClientSystemProperties(String cacheConfiguration) {
        setExtendClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for an Extend client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
    @Deprecated
    public static void setExtendClientSystemProperties(String cacheConfiguration,
                                                       Properties properties) {

        PropertyContainer containerToUse = internalCreateExtendClientPropertyContainerWithDefaults();
        containerToUse.addProperties(properties);

        internalSetGenericClientSystemProperties(cacheConfiguration, containerToUse);
    }

    @Deprecated
    private static void internalSetGenericClientSystemProperties(String cacheConfiguration,
                                                                 PropertyContainer propertyContainer) {

        PropertyContainer containerToUse = new PropertyContainer(propertyContainer);
        containerToUse.addProperty(CoherenceSystemPropertyConst.CACHECONFIG_KEY, cacheConfiguration);

        outputAndSetClientSystemProperties(containerToUse);
    }


    @Deprecated
    private static void outputAndSetClientSystemProperties(PropertyContainer propertyContainer) {
        LOGGER.fine(format("Client current Coherence properties: %s ",
                SystemUtils.getSystemPropertiesWithPrefix(CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT)));

        LOGGER.fine(format("Client system properties to set: %s", propertyContainer));

        SystemUtils.setReplaceClearSystemProperties(propertyContainer);
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createStorageDisabledClientPropertiesWithDefaults() {
        return internalCreateStorageDisabledClientPropertyContainerWithDefaults().getProperties();

    }

    @Deprecated
    private static PropertyContainer internalCreateStorageDisabledClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(DefaultClusterMemberGroupBuilder.createCacheServerPropertiesWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.ROLE_KEY, "StorageDisabledClient");
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());

        return container;
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    @Deprecated
    public static Properties createExtendClientPropertiesWithDefaults() {
        return internalCreateExtendClientPropertyContainerWithDefaults().getProperties();
    }

    @Deprecated
    private static PropertyContainer internalCreateExtendClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(DefaultClusterMemberGroupBuilder.createGenericClusterMemberPropertiesWithDefaults());
        container.addProperty(CoherenceSystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());
        container.addProperty(CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        container.addProperty(CoherenceSystemPropertyConst.TCMP_ENABLED_KEY, Boolean.FALSE.toString());

        return container;
    }
}
