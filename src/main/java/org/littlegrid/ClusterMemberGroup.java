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

/**
 * Cluster member group, a collection of cluster members enabling them to be controlled as a
 * group or individually for some operations.
 */
public interface ClusterMemberGroup {
    /**
     * Shuts down specific cluster members in the group, the members shutdown politely and notify
     * the other members as they leave..
     *
     * @param memberIds Ids of cluster member(s) to shutdown.
     * @return member group.
     */
    ClusterMemberGroup shutdownMember(int... memberIds);

    /**
     * Shuts down all the cluster members in the group, the members shutdown politely and notify
     * the other members as each one leaves.
     *
     * @return member group.
     */
    ClusterMemberGroup shutdownAll();

    /**
     * Returns <b>true</b> if the cluster member group has had shutdownAll invoked on it.  Once
     * shutdownAll has been called, then the cluster member group is deemed to be all shutdown.
     *
     * @return true if all shutdown.
     * @since 2.15
     */
    boolean isAllShutdown();

    /**
     * Stops specific cluster members immediately - this is typically used in-conjunction
     * getSuggestedSleepAfterStopDuration and TimeUnit.SECONDS, the members leave without notifying
     * other members.  An example of usage might be:
     * <code>
     * memberGroup.stop(2);
     * TimeUnit.SECONDS.sleep(memberGroup.getSuggestedSleepAfterStopDuration());
     * </code>
     *
     * @param memberIds Ids of cluster member(s) to stop,
     * @return member group.
     */
    ClusterMemberGroup stopMember(int... memberIds);

    /**
     * Stops all the cluster members in the group immediately, the members leave without notifying
     * the other members.
     *
     * @return member group.
     */
    ClusterMemberGroup stopAll();

    /**
     * Returns a specific cluster member.
     *
     * @param memberId Member id
     * @return member or <b>null</b> if called on a group that hasn't or can't start-up.
     */
    ClusterMember getClusterMember(int memberId);

    /**
     * Returns the member Ids of started cluster members.
     *
     * @return members Ids.
     */
    int[] getStartedMemberIds();

    /**
     * Returns the suggested seconds to sleep after performing a member stop, the sleep
     * time is dependent upon the version of Coherence - this is typically used in-conjunction
     * stop and TimeUnit.SECONDS.  An example of usage might be:
     * <code>
     * memberGroup.stop(2);
     * TimeUnit.SECONDS.sleep(memberGroup.getSuggestedSleepAfterStopDuration());
     * </code>
     *
     * @return seconds to sleep.
     */
    int getSuggestedSleepAfterStopDuration();

    /**
     * Merge in a cluster member group with this cluster member group.
     *
     * @param memberGroup Cluster member group to be merged.
     * @return new size of combined member group.
     * @since 2.7
     */
    int merge(ClusterMemberGroup memberGroup);

    /**
     * Returns the class loaders into which the the cluster members have been loaded - only
     * class loaders belonging to valid member ids are returned.
     *
     * @param memberIds Member ids.
     * @return class loaders.
     * @since 2.13
     */
    ClassLoader[] getActualContainingClassLoaders(int... memberIds);

    /**
     * Returns the well-known address port that the cluster member group had used to establish
     * the cluster - this is useful for working out what value was and then subsequently
     * setting it to a different number when running multiple autonomous clusters.
     *
     * @return WKA port.
     * @since 2.14
     */
    int getWkaPort();

    /**
     * Returns the Extend proxy port, for working out what the default value is and then
     * subsequently setting it to a different number when running multiple proxy servers.
     *
     * @return Extend port.
     * @since 2.14
     */
    int getExtendPort();
}
