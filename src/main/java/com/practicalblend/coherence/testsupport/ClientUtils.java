package com.practicalblend.coherence.testsupport;

import com.practicalblend.coherence.testsupport.server.impl.util.PropertyContainer;
import com.practicalblend.coherence.testsupport.server.impl.util.SystemUtils;
import org.apache.log4j.Logger;

import java.util.Properties;

import static java.lang.String.format;
import static com.practicalblend.coherence.testsupport.ServerFactory.createCacheServerPropertiesWithDefaults;
import static com.practicalblend.coherence.testsupport.ServerFactory.createGenericClusterMemberPropertiesWithDefaults;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.CACHECONFIG_KEY;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.DISTRIBUTED_LOCALSTORAGE_KEY;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.EXTEND_ENABLED_KEY;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.ROLE_KEY;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.TANGOSOL_COHERENCE_DOT;
import static com.practicalblend.coherence.testsupport.SystemPropertyConst.TCMP_ENABLED_KEY;

/**
 * Coherence client utils.
 */
public class ClientUtils {
    private static Logger LOGGER = Logger.getLogger(ClientUtils.class);

    /**
     * Private constructor to prevent creation.
     */
    private ClientUtils() {
    }

    /**
     * Sets the system properties for a storage-disabled client.
     */
    public static void setStorageDisabledClientSystemProperties() {
        setStorageDisabledClientSystemProperties(null, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     */
    public static void setStorageDisabledClientSystemProperties(String cacheConfiguration) {
        setStorageDisabledClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for a storage-disabled client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
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
    public static void setExtendClientSystemProperties(String cacheConfiguration) {
        setExtendClientSystemProperties(cacheConfiguration, null);
    }

    /**
     * Sets the system properties for an Extend client.
     *
     * @param cacheConfiguration Cache configuration.
     * @param properties         Properties.
     */
    public static void setExtendClientSystemProperties(String cacheConfiguration,
                                                       Properties properties) {

        PropertyContainer containerToUse = internalCreateExtendClientPropertyContainerWithDefaults();
        containerToUse.addProperties(properties);

        internalSetGenericClientSystemProperties(cacheConfiguration, containerToUse);
    }

    private static void internalSetGenericClientSystemProperties(String cacheConfiguration,
                                                                 PropertyContainer propertyContainer) {

        PropertyContainer containerToUse = new PropertyContainer(propertyContainer);
        containerToUse.addProperty(CACHECONFIG_KEY, cacheConfiguration);

        outputAndSetClientSystemProperties(containerToUse);
    }


    private static void outputAndSetClientSystemProperties(PropertyContainer propertyContainer) {
        LOGGER.debug(format("Client current Coherence properties: %s ",
                SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));

        LOGGER.info(format("Client system properties to set: %s", propertyContainer));

        SystemUtils.setReplaceClearSystemProperties(propertyContainer);
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    public static Properties createStorageDisabledClientPropertiesWithDefaults() {
        return internalCreateStorageDisabledClientPropertyContainerWithDefaults().getProperties();

    }

    private static PropertyContainer internalCreateStorageDisabledClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(createCacheServerPropertiesWithDefaults());
        container.addProperty(ROLE_KEY, "StorageDisabledClient");
        container.addProperty(DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());

        return container;
    }

    /**
     * Creates properties for storage disabled client.
     *
     * @return properties.
     */
    public static Properties createExtendClientPropertiesWithDefaults() {
        return internalCreateExtendClientPropertyContainerWithDefaults().getProperties();
    }

    private static PropertyContainer internalCreateExtendClientPropertyContainerWithDefaults() {
        PropertyContainer container = new PropertyContainer(createGenericClusterMemberPropertiesWithDefaults());
        container.addProperty(DISTRIBUTED_LOCALSTORAGE_KEY, Boolean.FALSE.toString());
        container.addProperty(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        container.addProperty(TCMP_ENABLED_KEY, Boolean.FALSE.toString());

        return container;
    }
}
