/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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

package org.littlegrid.management;

import org.junit.Before;
import org.junit.Test;
import org.littlegrid.AbstractAfterTestShutdownIntegrationTest;
import org.littlegrid.ClusterMemberGroupUtils;

import javax.management.MBeanServerConnection;
import java.lang.management.ManagementFactory;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Management service utils integration tests.
 */
public class ManagementUtilsIntegrationTest extends AbstractAfterTestShutdownIntegrationTest {
    @Before
    public void beforeTest() {
        memberGroup = ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1)
                .setExtendProxyCount(1)
                .setJmxMonitorCount(1)
                .buildAndConfigureForNoClient();
    }

    @Test
    public void whatever() {
        final MBeanServerConnection connection = ManagementFactory.getPlatformMBeanServer();

        final ManagementService managementService = ManagementUtils.newBuilder()
                .build(connection);

        final TabularResult result = managementService.findManagementInformation(
                "select count() from Coherence:type=Node,*");

        assertThat(result.getRowCount(), is(1));
        assertThat(result.getColumnCount(), is(1));

        final Map<String, Object> resultRow = result.getRows().iterator().next();
        assertThat((Integer) resultRow.values().iterator().next(), is(3));
    }
}
