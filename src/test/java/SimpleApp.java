import com.practicalblend.coherence.testsupport.server.ClusterMemberGroup;
import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;

import static com.practicalblend.coherence.testsupport.ClientUtils.setStorageDisabledClientSystemProperties;
import static com.practicalblend.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static com.practicalblend.coherence.testsupport.ServerFactory.shutdownCacheFactoryThenClusterMemberGroups;

public class SimpleApp {
    public static void main(String[] args) {
        final String key = "123";

        ClusterMemberGroup memberGroup = null;

        try {
            memberGroup = createCacheServerGroup(2);

            setStorageDisabledClientSystemProperties();

            NamedCache cache = CacheFactory.getCache("test");
            cache.put(key, "Whatever");
            System.out.println(cache.get(key));
        } finally {
            shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
        }
    }
}
