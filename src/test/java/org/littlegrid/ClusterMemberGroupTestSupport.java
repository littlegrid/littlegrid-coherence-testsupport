/*
 * Copyright (c) 2010-2020 Jonathan Hall.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * Neither the name of the littlegrid nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.littlegrid;

import com.tangosol.net.CacheFactory;
import com.tangosol.net.Cluster;
import com.tangosol.net.Member;

import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Class containing constants and utility methods for cluster member group tests.
 */
public final class ClusterMemberGroupTestSupport {
    public static final Logger LOGGER = Logger.getLogger(ClusterMemberGroupTestSupport.class.getName());

    public static final String TCMP_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE =
            "coherence/littlegrid-test-cache-config.xml";

    public static final String TCMP_CUSTOM_CONFIGURED_CLUSTER_MEMBER_CACHE_CONFIGURATION_FILE =
            "coherence/littlegrid-test-custom-configured-cache-config.xml";

    public static final String EXTEND_CLIENT_CACHE_CONFIGURATION_FILE =
            "coherence/littlegrid-test-extend-client-cache-config.xml";

    public static final String CLIENT_OVERRIDE_CONFIGURATION_FILE =
            "coherence/littlegrid-test-client-override-config.xml";

    public static final String KNOWN_TEST_CACHE = "known-cache";
    public static final String KNOWN_EXTEND_TEST_CACHE = "known-extend-cache";
    public static final String INVOCATION_SERVICE_NAME = "InvocationService";

    public static final String KNOWN_MEMBER_NAME = "MemberNameFromOverrideFile";

    public static final int CLUSTER_SIZE_WITHOUT_CLUSTER_MEMBER_GROUP = 1;
    public static final int SINGLE_TEST_CLUSTER_SIZE = 1;
    public static final int SMALL_TEST_CLUSTER_SIZE = 2;
    public static final int MEDIUM_TEST_CLUSTER_SIZE = 3;
    public static final int LARGE_TEST_CLUSTER_SIZE = 6;

    private static final int NUMBER_OF_TIMES_TO_ASSERT_SIZE_BEFORE_GIVING_UP = 2;

    /**
     * Private constructor to prevent creation.
     */
    private ClusterMemberGroupTestSupport() {
    }

    public static void sleepForSeconds(final int seconds) {
        LOGGER.info(format(
                "Coherence '%s' - so will now sleep for %s seconds to allow the member left to be acknowledged",
                CacheFactory.VERSION, seconds));

        try {
            SECONDS.sleep(seconds);
        } catch (InterruptedException e) {
            throw new IllegalStateException(e);
        }
    }

    public static boolean doesMemberExist(final Cluster cluster,
                                          final int specifiedMemberId) {

        for (Object object : cluster.getMemberSet()) {
            Member member = (Member) object;

            if (member.getId() == specifiedMemberId) {
                return true;
            }
        }

        return false;
    }

    public static void assertThatClusterIsExpectedSize(final Cluster cluster,
                                                       final int expectedClusterSize) {

        int currentClusterSize = cluster.getMemberSet().size();
        boolean expectedSizeOrTimedOutWaiting = (currentClusterSize == expectedClusterSize);
        int counter = 0;

        while (!expectedSizeOrTimedOutWaiting) {
            counter++;

            if (counter == (NUMBER_OF_TIMES_TO_ASSERT_SIZE_BEFORE_GIVING_UP + 1)) {
                expectedSizeOrTimedOutWaiting = true;
            } else {
                LOGGER.warning(format("Cluster size is presently %d, but was expected to be %d - will check again "
                        + "after sleeping, the cluster could be stabilising after a failover.  Number of attempts "
                        + "%d out of %d before giving up",
                        currentClusterSize, expectedClusterSize,
                        counter, NUMBER_OF_TIMES_TO_ASSERT_SIZE_BEFORE_GIVING_UP));

                try {
                    TimeUnit.SECONDS.sleep(1);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }

                currentClusterSize = cluster.getMemberSet().size();
                expectedSizeOrTimedOutWaiting = (currentClusterSize == expectedClusterSize);
            }
        }

        assertThat("Cluster is not the expected size", cluster.getMemberSet().size(), is(expectedClusterSize));
    }
}
