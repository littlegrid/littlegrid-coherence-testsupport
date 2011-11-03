package org.testsupport.coherence;

/**
 * Coherence system property constants.
 */
public class CoherenceSystemPropertyConst {
    /**
     * Private constructor to prevent creation.
     */
    private CoherenceSystemPropertyConst() {
    }

    public static final String TANGOSOL_COHERENCE_DOT = "tangosol.coherence.";

    public static final String LOG_LEVEL_KEY = TANGOSOL_COHERENCE_DOT + "log.level";
    public static final String ROLE_KEY = TANGOSOL_COHERENCE_DOT + "role";
    public static final String DISTRIBUTED_LOCALSTORAGE_KEY = TANGOSOL_COHERENCE_DOT + "distributed.localstorage";
    public static final String EXTEND_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "extend.enabled";
    public static final String TCMP_ENABLED_KEY = TANGOSOL_COHERENCE_DOT + "tcmp.enabled";
    public static final String WKA_KEY = TANGOSOL_COHERENCE_DOT + "wka";
    public static final String WKA_PORT_KEY = TANGOSOL_COHERENCE_DOT + "wka.port";
    public static final String LOCALHOST_KEY = TANGOSOL_COHERENCE_DOT + "localhost";
    public static final String LOCAL_PORT_KEY = TANGOSOL_COHERENCE_DOT + "localport";
    public static final String TTL_KEY = TANGOSOL_COHERENCE_DOT + "ttl";
    public static final String LOG_KEY = TANGOSOL_COHERENCE_DOT + "log";
    public static final String CACHECONFIG_KEY = TANGOSOL_COHERENCE_DOT + "cacheconfig";
    public static final String CLUSTER_KEY = TANGOSOL_COHERENCE_DOT + "cluster";

    @Deprecated
    public static final String MANAGEMENT_KEY = TANGOSOL_COHERENCE_DOT + "management";

    @Deprecated
    public static final String MANAGEMENT_REMOTE_KEY = "management.remote";

    @Deprecated
    public static final String JMXREMOTE_KEY = "com.sun.management.jmxremote";

    @Deprecated
    public static final int DEFAULT_WKA_PORT = 12051;

    @Deprecated
    public static final int DEFAULT_LOG_LEVEL = 3;

    @Deprecated
    public static final String DEFAULT_LOG = "log4j";
}
