package org.testsupport.coherence;

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
     * Constant for defining standard: tangosol.coherence
     */
    public static final String TANGOSOL_COHERENCE_DOT = "tangosol.coherence.";

    /**
     * Constant for standard log level system property.
     */
    public static final String LOG_LEVEL_KEY = TANGOSOL_COHERENCE_DOT + "log.level";
    public static final String ROLE_NAME_KEY = TANGOSOL_COHERENCE_DOT + "role";
    public static final String DISTRIBUTED_LOCAL_STORAGE_KEY = TANGOSOL_COHERENCE_DOT + "distributed.localstorage";
    public static final String EXTEND_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "extend.enabled";
    public static final String EXTEND_PORT_KEY = TANGOSOL_COHERENCE_DOT + "extend.port";
    public static final String TCMP_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "tcmp.enabled";
    public static final String WKA_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "wka";
    public static final String WKA_PORT_KEY = TANGOSOL_COHERENCE_DOT + "wka.port";
    public static final String LOCAL_ADDRESS_KEY = TANGOSOL_COHERENCE_DOT + "localhost";
    public static final String LOCAL_PORT_KEY = TANGOSOL_COHERENCE_DOT + "localport";
    public static final String TTL_KEY = TANGOSOL_COHERENCE_DOT + "ttl";
    public static final String LOG_KEY = TANGOSOL_COHERENCE_DOT + "log";
    public static final String CACHE_CONFIGURATION_KEY = TANGOSOL_COHERENCE_DOT + "cacheconfig";
    public static final String OVERRIDE_KEY = TANGOSOL_COHERENCE_DOT + "override";
    public static final String CLUSTER_KEY = TANGOSOL_COHERENCE_DOT + "cluster";

    @Deprecated
    public static final String MANAGEMENT_KEY = TANGOSOL_COHERENCE_DOT + "management";

    @Deprecated
    public static final String MANAGEMENT_REMOTE_KEY = "management.remote";

    @Deprecated
    public static final String JMXREMOTE_KEY = "com.sun.management.jmxremote";
}
