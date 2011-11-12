package org.littlegrid.coherence.testsupport;

/**
 * Coherence system property constants.
 */
public final class CoherenceSystemPropertyConst {
    /**
     * Private constructor to prevent creation.
     */
    private CoherenceSystemPropertyConst() {
    }

    /**
     * Constant for defining standard: tangosol.coherence.
     */
    public static final String TANGOSOL_COHERENCE_DOT = "tangosol.coherence.";

    /**
     * Constant for standard log level system property.
     */
    public static final String LOG_LEVEL_KEY = TANGOSOL_COHERENCE_DOT + "log.level";

    /**
     * Constant for standard role property.
     */
    public static final String ROLE_NAME_KEY = TANGOSOL_COHERENCE_DOT + "role";

    /**
     * Constant for standard distributed local storage property.
     */
    public static final String DISTRIBUTED_LOCAL_STORAGE_KEY = TANGOSOL_COHERENCE_DOT + "distributed.localstorage";

    /**
     * Constant for standard Extend enabled property.
     */
    public static final String EXTEND_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "extend.enabled";

    /**
     * Constant for standard Extend port property.
     */
    public static final String EXTEND_PORT_KEY = TANGOSOL_COHERENCE_DOT + "extend.port";

    /**
     * Constant for standard TCMP enabled property.
     */
    public static final String TCMP_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "tcmp.enabled";

    /**
     * Constant for standard WKA address property.
     */
    public static final String WKA_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "wka";

    /**
     * Constant for standard WKA port property.
     */
    public static final String WKA_PORT_KEY = TANGOSOL_COHERENCE_DOT + "wka.port";

    /**
     * Constant for standard local address property.
     */
    public static final String LOCAL_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "localhost";

    /**
     * Constant for standard local port property.
     */
    public static final String LOCAL_PORT_KEY = TANGOSOL_COHERENCE_DOT + "localport";

    /**
     * Constant for standard TTL property.
     */
    public static final String TTL_KEY = TANGOSOL_COHERENCE_DOT + "ttl";

    /**
     * Constant for standard log property.
     */
    public static final String LOG_KEY = TANGOSOL_COHERENCE_DOT + "log";

    /**
     * Constant for standard cache configuration property.
     */
    public static final String CACHE_CONFIGURATION_KEY = TANGOSOL_COHERENCE_DOT + "cacheconfig";

    /**
     * Constant for standard override configuration property.
     */
    public static final String OVERRIDE_KEY = TANGOSOL_COHERENCE_DOT + "override";

    /**
     * Constant for standard cluster configuration property.
     */
    public static final String CLUSTER_NAME_KEY = TANGOSOL_COHERENCE_DOT + "cluster";

    @Deprecated
    public static final String MANAGEMENT_KEY = TANGOSOL_COHERENCE_DOT + "management";

    @Deprecated
    public static final String MANAGEMENT_REMOTE_KEY = "management.remote";

    @Deprecated
    public static final String JMXREMOTE_KEY = "com.sun.management.jmxremote";
}
