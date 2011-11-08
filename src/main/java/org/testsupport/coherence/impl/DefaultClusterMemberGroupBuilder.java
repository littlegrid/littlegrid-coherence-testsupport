package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.LoggerPlaceHolder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import static java.lang.String.format;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.CACHE_CONFIGURATION_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.DISTRIBUTED_LOCAL_STORAGE_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_ADDRESS_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOCAL_PORT_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.LOG_LEVEL_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.OVERRIDE_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.ROLE_NAME_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TTL_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_ADDRESS_KEY;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final String DEFAULT_PROPERTIES_FILENAME = "testsupport.coherence.default.properties";
    private static final String OVERRIDE_PROPERTIES_FILENAME = "testsupport.coherence.override.properties";
    private static final LoggerPlaceHolder LOGGER =
            new LoggerPlaceHolder(DefaultClusterMemberGroupBuilder.class.getName());

    private Properties systemProperties = new Properties();
    private int storageEnabledCount;
    private int extendProxyCount;
    private int storageEnabledExtendProxyCount;
    private String cacheConfiguration;
    private String overrideConfiguration;
    private int wkaPort;
    private int localPort = wkaPort;
    private String wkaAddress;
    private String localAddress = wkaAddress;
    private String clusterMemberInstanceClassName;
    private int numberOfThreadsInStartUpPool;
    private int logLevel;
    private String[] jarsToExcludeFromClassPath;
    private String storageEnabledRoleName;
    private String storageDisabledClientRoleName;
    private URL[] classPathUrls;
    private String clientCacheConfiguration;
    private String extendProxyRoleName;
    private String storageEnabledExtendProxyRoleName;
    private String ttl;


    //TODO: Think about JMX
//            properties.addSystemProperty(MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMXREMOTE_KEY, "");

    public String getStorageEnabledRoleName() {
        return storageEnabledRoleName;
    }

    public ClusterMemberGroup.Builder setStorageEnabledRoleName(String storageEnabledRoleName) {
        this.storageEnabledRoleName = storageEnabledRoleName;

        return this;
    }

    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(String roleName) {
        this.storageEnabledExtendProxyRoleName = roleName;

        return this;
    }

    public ClusterMemberGroup.Builder setExtendProxyRoleName(String roleName) {
        this.extendProxyRoleName = roleName;

        return this;
    }

    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(String roleName) {
        this.storageDisabledClientRoleName = roleName;

        return this;
    }

    public String getTtl() {
        return ttl;
    }

    public ClusterMemberGroup.Builder setTtl(String ttl) {
        this.ttl = ttl;

        return this;
    }

    /**
     * Default constructor.
     */
    public DefaultClusterMemberGroupBuilder() {
        loadAndProcessProperties();
    }

    private void loadAndProcessProperties() {
        try {
            Properties defaultProperties = new Properties();
            defaultProperties.load(this.getClass().getClassLoader().getResourceAsStream(DEFAULT_PROPERTIES_FILENAME));

            //TODO: ADD SUPPORT TO LOAD OVERRIDE PROPERTIES
//            Properties overrideProperties = new Properties();
//            overrideProperties.load(this.getClass().getClassLoader().getResourceAsStream(OVERRIDE_PROPERTIES_FILENAME));

            Properties properties = new Properties(defaultProperties);
//            properties.putAll(overrideProperties);

            BeanUtils.processProperties(this, properties);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private void setSystemPropertyWhenValid(final String key,
                                            final String value) {

        if (key != null && value != null && !value.isEmpty()) {
            systemProperties.setProperty(key, value);
        }
    }

    @Override
    public ClusterMemberGroup build() {
        DefaultLocalProcessClusterMemberGroup containerGroup = new DefaultLocalProcessClusterMemberGroup();

        if (storageEnabledCount == 0 && storageEnabledExtendProxyCount == 0 && extendProxyCount == 0) {
            storageEnabledCount = 1;
        }

        if (classPathUrls == null) {
            LOGGER.fine("Cluster member group config class path URLs null, setting to current (minus Java home)");

            this.classPathUrls = getClassPathUrlsExcludingJavaHome(jarsToExcludeFromClassPath);
        }

        if (storageEnabledCount > 0) {
            preparePropertiesForStorageEnabled();

            ClusterMemberGroup memberGroup = new DefaultLocalProcessClusterMemberGroup(storageEnabledCount, systemProperties,
                    classPathUrls, jarsToExcludeFromClassPath, clusterMemberInstanceClassName,
                    numberOfThreadsInStartUpPool).startAll();

            containerGroup.merge((DefaultLocalProcessClusterMemberGroup) memberGroup);
        }

        if (extendProxyCount > 0) {
            preparePropertiesForExtendProxy();

            ClusterMemberGroup memberGroup = new DefaultLocalProcessClusterMemberGroup(extendProxyCount, systemProperties,
                    classPathUrls, jarsToExcludeFromClassPath, clusterMemberInstanceClassName,
                    numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge((DefaultLocalProcessClusterMemberGroup) memberGroup);
        }

        if (storageEnabledExtendProxyCount > 0) {
            preparePropertiesForStorageEnabledExtendProxy();

            ClusterMemberGroup memberGroup = new DefaultLocalProcessClusterMemberGroup(
                    storageEnabledExtendProxyCount, systemProperties, classPathUrls,
                    jarsToExcludeFromClassPath, clusterMemberInstanceClassName, numberOfThreadsInStartUpPool)
                    .startAll();

            containerGroup.merge((DefaultLocalProcessClusterMemberGroup) memberGroup);
        }

        systemProperties.clear();

        if (storageEnabledExtendProxyCount > 0 || extendProxyCount > 0) {
            preparePropertiesForExtendProxyClient();
        } else {
            preparePropertiesForStorageDisabledClient();
        }

        LOGGER.info(SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));
        LOGGER.info(systemProperties);
        SystemUtils.applyToSystemProperties(systemProperties);

        return containerGroup;
    }

    private void preparePropertiesForStorageEnabled() {
        setSystemPropertyWhenValid(WKA_ADDRESS_KEY, wkaAddress);
        setSystemPropertyWhenValid(LOCAL_ADDRESS_KEY, localAddress);
        setSystemPropertyWhenValid(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemPropertyWhenValid(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageEnabledRoleName);

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(TTL_KEY, ttl);
        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());
    }

    private void preparePropertiesForExtendProxy() {
        setSystemPropertyWhenValid(WKA_ADDRESS_KEY, wkaAddress);
        setSystemPropertyWhenValid(LOCAL_ADDRESS_KEY, localAddress);
        setSystemPropertyWhenValid(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemPropertyWhenValid(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemPropertyWhenValid(ROLE_NAME_KEY, extendProxyRoleName);

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(TTL_KEY, ttl);
        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());

        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
    }

    private void preparePropertiesForStorageEnabledExtendProxy() {
        setSystemPropertyWhenValid(WKA_ADDRESS_KEY, wkaAddress);
        setSystemPropertyWhenValid(LOCAL_ADDRESS_KEY, localAddress);
        setSystemPropertyWhenValid(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemPropertyWhenValid(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageEnabledExtendProxyRoleName);

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(TTL_KEY, ttl);
        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());

        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
    }

    private void preparePropertiesForStorageDisabledClient() {
        //TODO: Add check for client specific configuration
        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageDisabledClientRoleName);
        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
    }

    private void preparePropertiesForExtendProxyClient() {
        //TODO: Add check for client specific configuration
        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(ROLE_NAME_KEY, "");
        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setCacheConfiguration(final String cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxySpecificCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledSpecificCacheConfiguration(final String cacheConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClientCacheConfiguration(final String cacheConfiguration) {
        this.clientCacheConfiguration = cacheConfiguration;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setOverrideConfiguration(final String overrideConfiguration) {
        this.overrideConfiguration = overrideConfiguration;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClientOverrideConfiguration(final String overrideConfiguration) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setSystemProperties(final Properties properties) {
        this.systemProperties = properties;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledCount(final int numberOfMembers) {
        this.storageEnabledCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getStorageEnabledCount() {
        return storageEnabledCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        this.storageEnabledExtendProxyCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */

    @Override
    public int getStorageEnabledExtendProxyCount() {
        return storageEnabledExtendProxyCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendProxyCount(final int numberOfMembers) {
        this.extendProxyCount = numberOfMembers;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getExtendProxyCount() {
        return extendProxyCount;
    }

    @Override
    public ClusterMemberGroup.Builder setLogLevel(final int logLevel) {
        this.logLevel = logLevel;

        return this;
    }

    @Override
    public int getLogLevel() {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setClusterMemberInstanceClassName(final String clusterMemberInstanceClassName) {
        this.clusterMemberInstanceClassName = clusterMemberInstanceClassName;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getClusterMemberInstanceClassName() {
        return clusterMemberInstanceClassName;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setJarsToExcludeFromClassPath(final String... jarsToExcludeFromClassPath) {
        this.jarsToExcludeFromClassPath = jarsToExcludeFromClassPath;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaAddress(final String wkaAddress) {
        this.wkaAddress = wkaAddress;
        this.localAddress = this.wkaAddress;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWkaAddress() {
        return wkaAddress;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setWkaPort(final int wkaPort) {
        this.wkaPort = wkaPort;
        this.localPort = this.wkaPort;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getWkaPort() {
        return wkaPort;
    }

    @Override
    public ClusterMemberGroup.Builder setBuilder(Properties properties) {
        throw new UnsupportedOperationException();
    }

    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;

        return this;
    }

    private static URL[] getClassPathUrlsExcludingJavaHome(final String... jarsToExcludeFromClassPath) {
        //TODO: Pull out the JAR exclusion code if this feature seems like it will be required
        String pathSeparator = System.getProperty("path.separator");
        String[] classPathArray = System.getProperty("java.class.path").split(pathSeparator);
        String javaHome = System.getProperty("java.home");

        List<URL> classPathUrls = new ArrayList<URL>();

        for (String partOfClassPath : classPathArray) {
            if (!partOfClassPath.startsWith(javaHome)) {
                boolean found = false;

                if (jarsToExcludeFromClassPath != null) {
                    for (String jarToExclude : jarsToExcludeFromClassPath) {
                        if (partOfClassPath.endsWith(jarToExclude)) {
                            LOGGER.fine(format("JAR: '%s' specified for exclusion from class path", jarToExclude));

                            found = true;
                        }
                    }
                }

                if (!found) {
                    try {
                        classPathUrls.add(new File(partOfClassPath).toURI().toURL());
                    } catch (MalformedURLException e) {
                        throw new IllegalStateException(e);
                    }
                }
            }
        }

        return classPathUrls.toArray(new URL[classPathUrls.size()]);
    }
}
