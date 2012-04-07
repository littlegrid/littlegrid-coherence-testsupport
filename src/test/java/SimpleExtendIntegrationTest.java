import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import com.tangosol.util.aggregator.Count;
import com.tangosol.util.filter.AlwaysFilter;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.util.Collections;

import static junit.framework.Assert.assertEquals;

/**
 * Simple Extend littlegrid integration tests.
 */
public class SimpleExtendIntegrationTest {
    private static final int NUMBER_OF_ITEMS = 250;
    private static ClusterMemberGroup memberGroup;

    private final NamedCache cache = CacheFactory.getCache("simple-extend-example");


    /**
     * Use before class to start the cluster up before any of the tests run, additionally
     * by using before class, the start-up delay only occurs once.
     * <p/>
     * Note: apart from shutting down littlegrid, its code and API shouldn't really be in
     * any of your tests, unless you want to perform a stop or shutdown of a particular member
     * for failover testing.
     */
    @BeforeClass
    public static void beforeTests() {
        /*
            Build the cluster

            Note: use of the fast-start option and log level set to 6, so that we see the
            interesting Extend logging of client connections.
         */
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(2)
                .setCacheConfiguration("simple-extend-cache-config.xml")
                .setExtendProxyCount(1)
                .setClientCacheConfiguration("simple-extend-client-cache-config.xml")
                .setFastStartJoinTimeoutMilliseconds(100)
                .setLogLevel(6)
                .buildAndConfigureForExtendClient();
    }

    /**
     * Shutdown the cluster, this method also does a CacheFactory.shutdown() for the client
     * to ensure that leaves nicely as well.
     */
    @AfterClass
    public static void afterTests() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    /**
     * For this example test we are clearing down the cache each time and repopulating it.
     */
    @Before
    public void beforeTest() {
        cache.clear();

        for (int i = 0; i < NUMBER_OF_ITEMS; i++) {
            cache.putAll(Collections.singletonMap(i, i));
        }

        assertEquals(NUMBER_OF_ITEMS, cache.size());
    }

    @Test
    public void putGet() {
        cache.put("key", "value");

        assertEquals(NUMBER_OF_ITEMS + 1, cache.size());
    }

    @Test
    public void aggregateCount() {
        assertEquals(NUMBER_OF_ITEMS, cache.aggregate(AlwaysFilter.INSTANCE, new Count()));
    }
}
