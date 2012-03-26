package org.littlegrid.group.storage_enabled;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.NamedCache;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import static org.littlegrid.ClusterMemberGroupTestSupport.KNOWN_TEST_CACHE;
import static org.littlegrid.ClusterMemberGroupTestSupport.TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE;

/**
 * Build for storage-enabled member integration test.
 */
public class BuildForStorageEnabledMemberIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Test
    public void whatever() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setCacheConfiguration(TCMP_CLUSTER_MEMBER_CACHE_CONFIG_FILE)
                .buildAndConfigureForStorageEnabledMember();

        final NamedCache cache = CacheFactory.getCache(KNOWN_TEST_CACHE);
        cache.put("key", "value");

    }
}
