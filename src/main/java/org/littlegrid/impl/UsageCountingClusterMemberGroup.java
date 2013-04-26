/*
 * Copyright (c) 2010-2013 Jonathan Hall.
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

package org.littlegrid.impl;

import org.littlegrid.ClusterMemberGroup;

import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.ReusableClusterMemberGroup;

/**
 * Usage counting local process cluster member group implementation, performs its
 * shutdown all only once its usage counter is zero.
 */
public class UsageCountingClusterMemberGroup extends DefaultClusterMemberGroup
        implements ReusableClusterMemberGroup {

    private static final Logger LOGGER = Logger.getLogger(UsageCountingClusterMemberGroup.class.getName());

    private int currentUsageCount = 0;
    private int peakUsageCount = 0;
    private int totalUsageCount = 0;

    /**
     * Constructor.
     *
     * @param callbackHandler               Callback handler.
     * @param sleepAfterStopDuration35x     Sleep duration for 3.5.x.
     * @param sleepAfterStopDuration36x     Sleep duration for 3.6.x.
     * @param sleepAfterStopDurationDefault Default sleep duration.
     * @param wkaPort                       WKA port.
     * @param extendPort                    Extend port.
     */
    public UsageCountingClusterMemberGroup(final CallbackHandler callbackHandler,
                                           final int sleepAfterStopDuration35x,
                                           final int sleepAfterStopDuration36x,
                                           final int sleepAfterStopDurationDefault,
                                           final int wkaPort,
                                           final int extendPort) {

        super(callbackHandler,
                sleepAfterStopDuration35x, sleepAfterStopDuration36x, sleepAfterStopDurationDefault,
                wkaPort, extendPort);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    ClusterMemberGroup startAll() {
        totalUsageCount++;

        currentUsageCount++;

        if (currentUsageCount > peakUsageCount) {
            peakUsageCount = currentUsageCount;
        }

        super.startAll();

        LOGGER.info(format("Start all invoked - current usage count is now: %d for '%s'", currentUsageCount, this));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownAll() {
        currentUsageCount--;

        LOGGER.info(format("Shutdown all invoked - current usage count is now: %d for '%s'", currentUsageCount, this));

        if (currentUsageCount == 0) {
            super.shutdownAll();
        } else {
            LOGGER.info("Deferring shutdown");
        }

        return this;
    }

    /**
     * Returns the current usage counter value.
     *
     * @return current count.
     */
    @Override
    public int getCurrentUsageCount() {
        return currentUsageCount;
    }

    /**
     * Returns the highest peak usage counter value.
     *
     * @return maximum count.
     */
    public int getPeakUsageCount() {
        return peakUsageCount;
    }

    /**
     * Returns the total number of times the instance has been used.
     *
     * @return reuse count.
     */
    public int getTotalUsageCount() {
        return totalUsageCount;
    }
}
