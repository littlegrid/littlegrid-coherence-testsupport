import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.jhall.coherence.testsupport.server.ClusterMemberGroup;

import static org.jhall.coherence.testsupport.ClientUtils.setStorageDisabledClientSystemProperties;
import static org.jhall.coherence.testsupport.ServerFactory.createCacheServerGroup;
import static org.jhall.coherence.testsupport.ServerFactory.shutdownCacheFactoryThenClusterMemberGroups;

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
