package org.littlegrid.coherence.impl;

import org.littlegrid.coherence.ClusterMemberGroup;
import org.littlegrid.common.LoggerPlaceHolder;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import static java.lang.String.format;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.CACHE_CONFIGURATION_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.DISTRIBUTED_LOCAL_STORAGE_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.EXTEND_ENABLED_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.EXTEND_PORT_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.LOCAL_ADDRESS_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.LOCAL_PORT_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.LOG_LEVEL_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.OVERRIDE_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.ROLE_NAME_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.TCMP_ENABLED_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.TTL_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.WKA_ADDRESS_KEY;
import static org.littlegrid.coherence.CoherenceSystemPropertyConst.WKA_PORT_KEY;

/**
 * Default cluster member group builder implementation.
 */
public final class DefaultClusterMemberGroupBuilder implements ClusterMemberGroup.Builder {
    private static final String DEFAULT_PROPERTIES_FILENAME = "littlegrid-builder-config-default.properties";
    private static final String OVERRIDE_PROPERTIES_FILENAME = "littlegrid-builder-config-override.properties";
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
    private Properties extendProxySpecificSystemProperties;
    private int extendPort;
    private String extendClientRoleName;
    private int ttl;


    //TODO: Think about JMX
//            properties.addSystemProperty(MANAGEMENT_KEY, "all");
//            properties.addSystemProperty(MANAGEMENT_REMOTE_KEY, "true");
//            properties.addSystemProperty(JMXREMOTE_KEY, "");

    /**
     * Sets the storage enabled member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageEnabledRoleName(final String roleName) {
        this.storageEnabledRoleName = roleName;

        return this;
    }

    /**
     * Sets the storage enabled Extend proxy member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyRoleName(final String roleName) {
        this.storageEnabledExtendProxyRoleName = roleName;

        return this;
    }

    /**
     * Sets the Extend proxy member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setExtendProxyRoleName(final String roleName) {
        this.extendProxyRoleName = roleName;

        return this;
    }

    /**
     * Sets the storage disabled member's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setStorageDisabledClientRoleName(final String roleName) {
        this.storageDisabledClientRoleName = roleName;

        return this;
    }

    /**
     * Sets the Extend client's role name.
     *
     * @param roleName Role name.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setExtendClientRoleName(final String roleName) {
        this.extendClientRoleName = roleName;

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
//            overrideProperties.load(this.getClass().getClassLoader().
// getResourceAsStream(OVERRIDE_PROPERTIES_FILENAME));

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

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup build() {
        //TODO: Tidy this up
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

            ClusterMemberGroup memberGroup =
                    new DefaultLocalProcessClusterMemberGroup(storageEnabledCount, systemProperties,
                            classPathUrls, jarsToExcludeFromClassPath, clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool).startAll();

            containerGroup.merge(memberGroup);
        }

        if (extendProxyCount == 1) {
            preparePropertiesForExtendProxy();

            ClusterMemberGroup memberGroup =
                    new DefaultLocalProcessClusterMemberGroup(extendProxyCount, systemProperties,
                            classPathUrls, jarsToExcludeFromClassPath, clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool)
                            .startAll();

            containerGroup.merge(memberGroup);
        } else if (extendProxyCount > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is currently supported");
        }

        if (storageEnabledExtendProxyCount == 1) {
            preparePropertiesForStorageEnabledExtendProxy();

            ClusterMemberGroup memberGroup =
                    new DefaultLocalProcessClusterMemberGroup(storageEnabledExtendProxyCount, systemProperties,
                            classPathUrls, jarsToExcludeFromClassPath, clusterMemberInstanceClassName,
                            numberOfThreadsInStartUpPool)
                            .startAll();

            containerGroup.merge(memberGroup);
        } else if (storageEnabledExtendProxyCount > 1) {
            throw new UnsupportedOperationException("Currently only one Extend proxy is currently supported");
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
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageEnabledRoleName);

        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));
    }

    private void preparePropertiesForExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(ROLE_NAME_KEY, extendProxyRoleName);

        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(EXTEND_PORT_KEY, Integer.toString(extendPort));

        if (extendProxySpecificSystemProperties != null) {
            systemProperties.putAll(extendProxySpecificSystemProperties);
        }
    }

    private void preparePropertiesForStorageEnabledExtendProxy() {
        preparePropertiesForTcmpClusterMember();

        setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, cacheConfiguration);
        setSystemPropertyWhenValid(OVERRIDE_KEY, overrideConfiguration);

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.TRUE.toString());

        setSystemPropertyWhenValid(LOG_LEVEL_KEY, Integer.toString(logLevel));

        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageEnabledExtendProxyRoleName);

        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(EXTEND_PORT_KEY, Integer.toString(extendPort));
    }

    private void preparePropertiesForStorageDisabledClient() {
        preparePropertiesForTcmpClusterMember();

        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());

        setSystemPropertyWhenValid(ROLE_NAME_KEY, storageDisabledClientRoleName);

        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
    }

    private void preparePropertiesForTcmpClusterMember() {
        setSystemPropertyWhenValid(TCMP_ENABLED_KEY, Boolean.TRUE.toString());
        setSystemPropertyWhenValid(WKA_ADDRESS_KEY, wkaAddress);
        setSystemPropertyWhenValid(LOCAL_ADDRESS_KEY, localAddress);
        setSystemPropertyWhenValid(WKA_PORT_KEY, Integer.toString(wkaPort));
        setSystemPropertyWhenValid(LOCAL_PORT_KEY, Integer.toString(localPort));
        setSystemPropertyWhenValid(TTL_KEY, Integer.toString(ttl));
    }

    private void preparePropertiesForExtendProxyClient() {
        if (clientCacheConfiguration != null) {
            setSystemPropertyWhenValid(CACHE_CONFIGURATION_KEY, clientCacheConfiguration);
        }

        setSystemPropertyWhenValid(DISTRIBUTED_LOCAL_STORAGE_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(TCMP_ENABLED_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(ROLE_NAME_KEY, extendClientRoleName);
        setSystemPropertyWhenValid(EXTEND_ENABLED_KEY, Boolean.FALSE.toString());
        setSystemPropertyWhenValid(EXTEND_PORT_KEY, Integer.toString(extendPort));
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
    public ClusterMemberGroup.Builder setExtendProxySpecificSystemProperties(final Properties properties) {
        this.extendProxySpecificSystemProperties = properties;

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
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final int numberOfMembers) {
        this.storageEnabledExtendProxyCount = numberOfMembers;

        return this;
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
    public ClusterMemberGroup.Builder setLogLevel(final int logLevel) {
        this.logLevel = logLevel;

        return this;
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

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setExtendPort(final int extendPort) {
        this.extendPort = extendPort;

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup.Builder setBuilderProperties(final Properties properties) {
        BeanUtils.processProperties(this, properties);

        return this;
    }

    /**
     * Sets the TTL.
     *
     * @param ttl TTL.
     * @return cluster member group builder.
     */
    public ClusterMemberGroup.Builder setTtl(final int ttl) {
        this.ttl = ttl;

        return this;
    }

    /**
     * Sets the number of threads to handle starting up the members within a cluster member group.
     *
     * @param numberOfThreadsInStartUpPool Number of threads available to start-up members.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final int numberOfThreadsInStartUpPool) {
        this.numberOfThreadsInStartUpPool = numberOfThreadsInStartUpPool;

        return this;
    }

    private static URL[] getClassPathUrlsExcludingJavaHome(final String... jarsToExcludeFromClassPath) {
        //TODO: Pull this out and add support for wildcards, e.g. *jmx*
        String pathSeparator = System.getProperty("path.separator");
        String[] classPathArray = System.getProperty("java.class.path").split(pathSeparator);
        String javaHome = System.getProperty("java.home");

        List<URL> classPathUrls = new ArrayList<URL>();

        for (String partOfClassPath : classPathArray) {
            if (!partOfClassPath.startsWith(javaHome)) {
                boolean includeInClassPath = true;

                if (jarsToExcludeFromClassPath != null) {
                    for (String jarToExclude : jarsToExcludeFromClassPath) {
                        if (partOfClassPath.endsWith(jarToExclude)) {
                            LOGGER.fine(format("JAR: '%s' specified for exclusion from class path", jarToExclude));

                            includeInClassPath = false;
                        }
                    }
                }

                if (includeInClassPath) {
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


    /*
     * *******************************************************************************************************
     * *******************************************************************************************************
     * *******************************************************************************************************
     * Below setter are required when using older versions of Coherence, such as 3.5.x - this is because the
     * reflection updater doesn't seem to set integer values.
     * <p/>
     * TODO: Look at why integer values don't get set.
     */


    /**
     * Required to support older version of Coherence.
     *
     * @param wkaPort WKA port.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setWkaPort(final String wkaPort) {
        setWkaPort(Integer.parseInt(wkaPort));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param numberOfMembers Number of members.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setStorageEnabledCount(final String numberOfMembers) {
        setStorageEnabledCount(Integer.parseInt(numberOfMembers));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param numberOfMembers Number of members.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setStorageEnabledExtendProxyCount(final String numberOfMembers) {
        setStorageEnabledExtendProxyCount(Integer.parseInt(numberOfMembers));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param numberOfMembers Number of members.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setExtendProxyCount(final String numberOfMembers) {
        setExtendProxyCount(Integer.parseInt(numberOfMembers));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param numberOfThreadsInStartUpPool Number of threads in start-up pool.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setNumberOfThreadsInStartUpPool(final String numberOfThreadsInStartUpPool) {
        setNumberOfThreadsInStartUpPool(Integer.parseInt(numberOfThreadsInStartUpPool));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param logLevel Log level.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setLogLevel(final String logLevel) {
        setLogLevel(Integer.parseInt(logLevel));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param extendPort Extend port.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setExtendPort(final String extendPort) {
        setExtendPort(Integer.parseInt(extendPort));

        return this;
    }

    /**
     * Required to support older version of Coherence.
     *
     * @param ttl TTL.
     * @return cluster member group.
     */
    public ClusterMemberGroup.Builder setTtl(final String ttl) {
        setTtl(Integer.parseInt(ttl));

        return this;
    }
}
