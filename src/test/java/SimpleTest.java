import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.testdg.coherence.support.ClusterMemberGroup;
import org.testdg.coherence.support.ClusterMemberGroupUtils;

import static org.junit.Assert.assertEquals;

/**
 * Simple test example.
 */
public class SimpleTest {
    private static ClusterMemberGroup memberGroup;

    @BeforeClass
    public static void beforeTests() {
        memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder()
                .setStorageEnabledCount(2).build();
    }

    @AfterClass
    public static void afterTests() {
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    @Test
    public void simpleExample() {
        NamedCache cache = CacheFactory.getCache("test");
        cache.put("key", "hello");

        assertEquals(1, cache.size());
    }
}
