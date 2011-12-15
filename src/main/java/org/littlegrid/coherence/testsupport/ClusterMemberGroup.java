/*
 * Copyright (c) 2011, Jonathan Hall.
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
 * Neither the name of the LittleGrid nor the names of its contributors may
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

package org.littlegrid.coherence.testsupport;

import java.util.List;
import java.util.Properties;

/**
 * Cluster member group, a collection of cluster members enabling them to be controlled as a
 * group or individually for some operations.
 */
public interface ClusterMemberGroup {
    /**
     * Shuts down specific cluster members in the group.
     *
     * @param memberIds Ids of cluster member(s) to shutdown.
     * @return member group.
     */
    ClusterMemberGroup shutdownMember(int... memberIds);

    /**
     * Shuts down all the cluster members in the group.
     *
     * @return member group.
     */
    ClusterMemberGroup shutdownAll();

    /**
     * Stops specific cluster members.
     *
     * @param memberIds Ids of cluster member(s) to stop.
     * @return member group.
     */
    ClusterMemberGroup stopMember(int... memberIds);

    /**
     * Stops all the cluster members in the group.
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
    List<Integer> getStartedMemberIds();

    /**
     * Builder interface for cluster member group.
     */
    interface Builder {
        /**
         * Builds and returns a <em>running cluster member group</em>, based upon the default
         * values and any values that have been overridden or explicitly set.
         *
         * @return running cluster member group.
         */
        ClusterMemberGroup build();

        /**
         * Sets the cache configuration to be used for the cluster member groups, this is used
         * for configurations where all TCMP members (storage enabled, Extend proxy and
         * storage disabled) use the same configuration then this will serve as the only cache
         * configuration file needing to be used (note: specific cache configurations can set
         * via other methods).
         *
         * @param cacheConfiguration Cache configuration.
         * @return cluster member group builder.
         */
        Builder setCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the cache configuration file to be used specifically for the storage enabled
         * members of the group.
         *
         * @param cacheConfiguration Cache configuration.
         * @return cluster member group builder.
         */
        Builder setStorageEnabledSpecificCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the cache configuration file to be used specifically for the Extend proxy
         * members of the group.
         *
         * @param cacheConfiguration Cache configuration.
         * @return cluster member group builder.
         */
        Builder setExtendProxySpecificCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the Coherence override file to be used for the cluster member groups, this is
         * used for configurations where all TCMP members (storage enabled, Extend proxy and
         * storage disabled) use the same configuration.
         *
         * @param overrideConfiguration Override configuration.
         * @return cluster member group builder.
         */
        Builder setOverrideConfiguration(String overrideConfiguration);

        /**
         * Sets the client specific cache configuration file, for instance Extend based clients.
         *
         * @param cacheConfiguration Cache configuration.
         * @return cluster member group builder.
         */
        Builder setClientCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the specific Coherence override file to be used, for instance when using Extend
         * based clients or when a client requires a different override file (perhaps because the
         * server cluster members need to be required with the SpringAwareCacheFactory).
         *
         * @param overrideConfiguration Override configuration.
         * @return cluster member group builder.
         */
        Builder setClientOverrideConfiguration(String overrideConfiguration);

        /**
         * Used to set any remaining system properties that are required, for instance if the
         * standard Coherence names for system properties aren't being used - i.e. a different
         * named property is used for enabling distributed local storage.
         *
         * @param properties Properties to be turned into system properties.
         * @return cluster member group builder.
         */
        Builder setSystemProperties(Properties properties);

        /**
         * Used to set any remaining Extend proxy specific system properties that are required, for
         * instance if the standard Coherence names for system properties aren't being used - i.e.
         * a different named property is used for enabling Extend.
         *
         * @param properties Properties to be turned into system properties.
         * @return cluster member group builder.
         */
        Builder setExtendProxySpecificSystemProperties(Properties properties);

        /**
         * Sets the number of storage enabled members (i.e. 'cache servers') the cluster member
         * group should contain.
         *
         * @param numberOfMembers Number of members required.
         * @return cluster member group builder.
         */
        Builder setStorageEnabledCount(int numberOfMembers);

        /**
         * Sets the number of storage enabled Extend proxy members (i.e. composite members running
         * Extend and as a 'cache server').
         *
         * @param numberOfMembers Number of members.
         * @return cluster member group builder.
         */
        Builder setStorageEnabledExtendProxyCount(int numberOfMembers);

        /**
         * Sets the number of Extend proxy members the cluster member group should contain.
         *
         * @param numberOfMembers Number of members.
         * @return cluster member group builder.
         */
        Builder setExtendProxyCount(int numberOfMembers);

        /**
         * Sets the log destination where the cluster members should output to.
         *
         * @param logDestination Log destination (standard Coherence log - stdout, log4j, java).
         * @return cluster member group builder.
         */
        Builder setLogDestination(String logDestination);

        /**
         * Sets the cluster name that the cluster members belong to.
         *
         * @param clusterName  Cluster name.
         * @return cluster member group builder.
         */
        Builder setClusterName(String clusterName);

        /**
         * Sets the log level that the cluster members should run with.
         *
         * @param logLevel Log level (standard Coherence 0-9).
         * @return cluster member group builder.
         */
        Builder setLogLevel(int logLevel);

        /**
         * Sets the cluster member instance class name, essentially instances of this class act as
         * cluster members - usually the default will suffice and this will not need to be changed,
         * examples where a different instance class name might be required are if specific control
         * is required before starting up a cluster member or shutting one down.
         *
         * @param clusterMemberInstanceClassName Class name from which instances of cluster members
         *                                       are created.
         * @return cluster member group builder.
         */
        Builder setClusterMemberInstanceClassName(String clusterMemberInstanceClassName);

        /**
         * Sets the names of specific JAR files to be excluded from the class loading of the
         * cluster members, this is useful if JMX JARs or JDBC drivers are resulting in (inert)
         * warning or error messages when starting the cluster member.
         *
         * @param jarsToExcludeFromClassPath Jars to be excluded, in the form of
         *                                   name-of-the-jar-to-exclude.jar
         * @return cluster member group builder.
         */
        Builder setJarsToExcludeFromClassPath(String... jarsToExcludeFromClassPath);

        /**
         * Sets the well-known address which is used to control which IP address/hostname that
         * Coherence should use - typically this value does not need to be changed, but in the
         * case of problems starting cluster members with the default then other IP addresses
         * on the machine can be tried.
         *
         * @param wkaAddress Well-known address, e.g. localhost or xxx.xxx.xxx.xxx IP address.
         * @return cluster member group builder.
         */
        Builder setWkaAddress(String wkaAddress);

        /**
         * Sets the well-known address port which is to be used, typically the default value will
         * suffice, but when the need to run with two separate autonomous clusters is required,
         * then this value will need to be changed for one of the clusters - otherwise all the
         * cluster members will simply join the same cluster.
         *
         * @param wkaPort Well-known port, e.g. 12345.
         * @return cluster member group builder.
         */
        Builder setWkaPort(int wkaPort);

        /**
         * Returns the well-known port, this is useful to working out what the default value is
         * and then subsequently setting it to a different number when running two autonomous
         * clusters.
         *
         * @return cluster member group builder.
         */
        int getWkaPort();

        /**
         * Sets the Extend port that the Extend proxy should listen on and for which the Extend
         * client should connect to the Extend proxy server.
         *
         * @param extendPort Extend port, e.g. 23451.
         * @return cluster member group builder.
         */
        Builder setExtendPort(int extendPort);

        /**
         * Sets the builder properties to be used, these will override any defaults - using
         * properties is useful if the configuration is required to be externalised, rather than
         * the builder being controlled through code.
         *
         * @param properties Properties containing overrides, the keys should match the methods
         *                   exposed on this cluster member group builder interface, minus the
         *                   'set' - so for example to set the WKA port, the entry in the properties
         *                   file would look like WkaPort=345612
         * @return cluster member group builder.
         */
        Builder setBuilderProperties(Properties properties);
    }
}
