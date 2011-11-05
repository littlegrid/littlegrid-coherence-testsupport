import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.coherence.ClusterMemberGroupUtils;

import static org.testsupport.coherence.ClusterMemberGroupUtils.setStorageDisabledClientSystemProperties;
import static org.testsupport.coherence.ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups;

public class SimpleApp {
    public static void main(String[] args) {
        final String key = "123";

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = ClusterMemberGroupUtils.newClusterMemberGroupBuilder().setStorageEnabledCount(2).build().startAll();

            setStorageDisabledClientSystemProperties();

            NamedCache cache = CacheFactory.getCache("test");
            cache.put(key, "hello");
            System.out.println(cache.get(key));
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }
}
