/*
 * Copyright (c) 2010-2012 Jonathan Hall.
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

import java.util.Map;
import java.util.Properties;

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
    int merge(final ClusterMemberGroup memberGroup);

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

    /**
     * Cluster member interface - implementations of this class need to provide basic functionality,
     * so they may be controlled by the {@link ClusterMemberGroup}
     * implementations - typically the default implementation of this class should suffice for most
     * uses.
     */
    public interface ClusterMember {
        /**
         * Shutdown the member, it leaves the cluster politely and notifies other members.
         */
        void shutdown();

        /**
         * Stops the member immediately, it leaves the cluster without notifying other members.
         */
        void stop();

        /**
         * Gets this local member Id.
         *
         * @return member id.
         */
        int getLocalMemberId();

        /**
         * Returns the class loader that the cluster member has been loaded into.
         *
         * @return class loader.
         */
        ClassLoader getActualContainingClassLoader();
    }

    /**
     * Builder interface for cluster member group.
     */
    interface Builder {
        /**
         * Constant defining the name of the system property that can be used to supply a different
         * override file by setting a system property with this name and the value being the
         * required override file to be use.
         */
        String BUILDER_OVERRIDE_KEY = "littlegrid.builder.override";

        /**
         * Constant defining the prefix to be used when configuring littlegrid via system
         * properties, for example: -Dlittlegrid.builder.ExtendPort=12345 - this configures the
         * Extend port via a system property.
         *
         * @since 2.13
         */
        String BUILDER_SYSTEM_PROPERTY_PREFIX_KEY = "littlegrid.builder.";

        /**
         * Constant defining the prefix to be used when configuring littlegrid via environment
         * variables, for example: export littlegrid_builder_ExtendPort=12345 (Unix/Linux) or
         * set littlegrid_builder_ExtendPort=12345 (Windows).
         *
         * @since 2.13
         */
        String BUILDER_ENVIRONMENT_VARIABLE_PREFIX_KEY = "littlegrid_builder_";

        /**
         * Constant defining the name of the system property that can be used to supply a different
         * mapping override file by setting a system property with this name and the value being the
         * required override file to be use.
         */
        String BUILDER_SYSTEM_PROPERTY_MAPPING_OVERRIDE_KEY = "littlegrid.builder.system.property.mapping.override";

        /**
         * Builds and returns a <em>running cluster member group</em>, based upon the default
         * values and any values that have been overridden or explicitly set - with this
         * build method <em>no system properties are set</em> for the client, if the client wants
         * to connect, then it will need to control its own setting of the system properties.
         *
         * @return running cluster member group.
         * @since 2.5.1 replaces build().
         */
        ClusterMemberGroup buildAndConfigureForNoClient();

        /**
         * Builds and returns a <em>running cluster member group</em>, based upon the default
         * values and any values that have been overridden or explicitly set - with this
         * build method, <em>system properties are then set</em> with the assumption that a storage-disabled
         * client wants to connect to the newly started cluster group.
         *
         * @return running cluster member group.
         * @since 2.5.1 replaces build().
         */
        ClusterMemberGroup buildAndConfigureForStorageDisabledClient();

        /**
         * Builds and returns a <em>running cluster member group</em>, based upon the default
         * values and any values that have been overridden or explicitly set - with this
         * build method, <em>system properties are then set</em> with the assumption that an Extend
         * client wants to connect to the newly started cluster group.
         *
         * @return running cluster member group.
         * @since 2.5.1 replaces build().
         */
        ClusterMemberGroup buildAndConfigureForExtendClient();

        /**
         * Builds and returns a <em>running cluster member group</em>, based upon the default
         * values and any values that have been overridden or explicitly set - with this
         * build method, <em>system properties are then set</em> with the assumption that a storage-enabled
         * member wants to connect to the newly started cluster group.
         *
         * @return running cluster member group.
         * @since 2.6
         */
        ClusterMemberGroup buildAndConfigureForStorageEnabledMember();

        /**
         * Sets the exception report instance class name.
         *
         * @param exceptionReportInstanceClassName
         *         Exception report instance name.
         * @return builder.
         */
        Builder setExceptionReporterInstanceClassName(String exceptionReportInstanceClassName);

        /**
         * Sets the cache configuration to be used for the cluster member groups, this is used
         * for configurations where all TCMP members (storage enabled, Extend proxy and
         * storage disabled) use the same configuration then this will serve as the only cache
         * configuration file needing to be used.
         *
         * @param cacheConfiguration Cache configuration.
         * @return builder.
         */
        Builder setCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the Coherence override file to be used for the cluster member groups, this is
         * used for configurations where all TCMP members (storage enabled, Extend proxy and
         * storage disabled) use the same configuration.
         *
         * @param overrideConfiguration Override configuration.
         * @return builder.
         */
        Builder setOverrideConfiguration(String overrideConfiguration);

        /**
         * Sets the client specific cache configuration file, for instance Extend based clients.
         *
         * @param cacheConfiguration Cache configuration.
         * @return builder.
         */
        Builder setClientCacheConfiguration(String cacheConfiguration);

        /**
         * Sets the specific Coherence override file to be used, for instance when using Extend
         * based clients or when a client requires a different override file - examples might be
         * when the server cluster members need something like the SpringAwareCacheFactory, but
         * that isn't required for the client.
         *
         * @param overrideConfiguration Override configuration.
         * @return builder.
         * @since 2.13
         */
        Builder setClientOverrideConfiguration(String overrideConfiguration);

        /**
         * Sets the cache configuration to be used for custom configured cluster member groups.
         *
         * @param cacheConfiguration Custom configured cache configuration.
         * @return builder.
         */
        Builder setCustomConfiguredCacheConfiguration(String cacheConfiguration);

        /**
         * Used to set any remaining system properties that are required when starting the cluster member
         * group.
         *
         * @param properties Properties to be turned into system properties.
         * @return builder.
         */
        Builder setAdditionalSystemProperties(Properties properties);

        /**
         * Used to set any remaining system properties that are required when starting the cluster
         * member group - multiple properties files are supported by passing as a comma separated string.
         *
         * @param commaDelimitedPropertiesFilenames
         *         String of properties filenames, multiple property
         *         files are supported.
         * @return builder.
         */
        Builder setAdditionalSystemProperties(String commaDelimitedPropertiesFilenames);

        /**
         * Used to set an additional system property that is required when starting the cluster
         * member group.
         *
         * @param key   System property key.
         * @param value System property value.
         * @return builder.
         * @since 2.8
         */
        Builder setAdditionalSystemProperty(String key,
                                            String value);

        /**
         * Used to set an additional system property that is required when starting the cluster
         * member group.
         *
         * @param key   System property key.
         * @param value System property value.
         * @return builder.
         * @since 2.13
         */
        Builder setAdditionalSystemProperty(String key,
                                            int value);

        /**
         * Used to set an additional system property that is required when starting the cluster
         * member group.
         *
         * @param key   System property key.
         * @param value System property value.
         * @return builder.
         * @since 2.13
         */
        Builder setAdditionalSystemProperty(String key,
                                            boolean value);

        /**
         * Sets the number of storage enabled members (i.e. 'cache servers') the cluster member
         * group should contain.
         *
         * @param numberOfMembers Number of members required.
         * @return builder.
         */
        Builder setStorageEnabledCount(int numberOfMembers);

        /**
         * Sets the number of custom configured members that the cluster member group should contain.
         *
         * @param numberOfMembers Number of members required.
         * @return builder.
         */
        Builder setCustomConfiguredCount(int numberOfMembers);

        /**
         * Sets the number of storage enabled Extend proxy members (i.e. composite members running
         * Extend and as a 'cache server').
         *
         * @param numberOfMembers Number of members.
         * @return builder.
         */
        Builder setStorageEnabledExtendProxyCount(int numberOfMembers);

        /**
         * Sets the number of Extend proxy members the cluster member group should contain.
         *
         * @param numberOfMembers Number of members.
         * @return builder.
         */
        Builder setExtendProxyCount(int numberOfMembers);

        /**
         * Sets the number of JMX monitor members the cluster member group should contain.
         *
         * @param numberOfMembers Number of members.
         * @return builder.
         */
        Builder setJmxMonitorCount(int numberOfMembers);

        /**
         * Sets the log destination where the cluster members should output to.
         *
         * @param logDestination Log destination (standard Coherence log - stdout, log4j, java).
         * @return builder.
         */
        Builder setLogDestination(String logDestination);

        /**
         * Sets the cluster name that the cluster members belong to.
         *
         * @param clusterName Cluster name.
         * @return builder.
         */
        Builder setClusterName(String clusterName);

        /**
         * Sets the log level that the cluster members should run with.
         *
         * @param logLevel Log level (standard Coherence 0-9).
         * @return builder.
         */
        Builder setLogLevel(int logLevel);

        /**
         * Sets the custom configured member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setCustomConfiguredRoleName(final String roleName);

        /**
         * Sets the storage enabled member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setStorageEnabledRoleName(final String roleName);

        /**
         * Sets the storage enabled Extend proxy member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setStorageEnabledExtendProxyRoleName(final String roleName);

        /**
         * Sets the Extend proxy member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setExtendProxyRoleName(final String roleName);

        /**
         * Sets the JMX monitor member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setJmxMonitorRoleName(final String roleName);

        /**
         * Sets the storage disabled member's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setStorageDisabledClientRoleName(final String roleName);

        /**
         * Sets the Extend client's role name.
         *
         * @param roleName Role name.
         * @return builder.
         */
        Builder setExtendClientRoleName(final String roleName);

        /**
         * Sets the TTL.
         *
         * @param ttl TTL.
         * @return builder.
         */
        Builder setTtl(final int ttl);

        /**
         * Sets the cluster member instance class name, essentially instances of this class act as
         * cluster members - usually the default will suffice and this will not need to be changed,
         * examples where a different instance class name might be required are if specific control
         * is required before starting up a cluster member or shutting one down.
         *
         * @param clusterMemberInstanceClassName Class name from which instances of cluster members
         *                                       are created.
         * @return builder.
         */
        Builder setClusterMemberInstanceClassName(String clusterMemberInstanceClassName);

        /**
         * Sets the cluster member instance class name, essentially instances of this class act as
         * cluster members - usually the default will suffice and this will not need to be changed,
         * examples where a different instance class name might be required are if specific control
         * is required before starting up a cluster member or shutting one down.
         *
         * @param clusterMemberInstanceClassName Class name from which instances of cluster members
         *                                       are created.
         * @return builder.
         */
        Builder setCustomConfiguredClusterMemberInstanceClassName(String clusterMemberInstanceClassName);

        /**
         * Sets the names of specific JAR files to be excluded from the class loading of the
         * cluster members, this is useful if JMX JARs or JDBC drivers are resulting in (inert)
         * warning or error messages when starting the cluster member.
         *
         * @param jarsToExcludeFromClassPath Jars to be excluded, in the form of:
         *                                   <i>name-of-the-jar-to-exclude.jar</i>
         *                                   or:
         *                                   <i>name-of-the-jar-to-exclude.jar,another-jar-to-exclude-1.1.jar</i>
         *                                   or abbreviated - useful to avoid specifying version numbers:
         *                                   <i>name-of,another</i>
         * @return builder.
         */
        Builder setJarsToExcludeFromClassPath(String... jarsToExcludeFromClassPath);

        /**
         * Sets the names of specific core JAR files (e.g. rt.jar) to be excluded from the class loading
         * of the cluster members, this is useful if the JAVA_HOME environment variable differs from the
         * version of Java being used in an IDE.
         *
         * @param coreJarsToExcludeFromClassPath Core Java Jars to be excluded, in the form of
         *                                       name-of-the-jar-to-exclude.jar
         * @return builder.
         * @since 2.11
         */
        Builder setCoreJarsToExcludeFromClassPath(String... coreJarsToExcludeFromClassPath);

        /**
         * Sets the well-known address which is used to control which IP address/hostname that
         * Coherence should use - typically this value does not need to be changed, but in the
         * case of problems starting cluster members with the default then other IP addresses
         * on the machine can be tried.
         *
         * @param wkaAddress Well-known address, e.g. localhost or xxx.xxx.xxx.xxx IP address.
         * @return builder.
         */
        Builder setWkaAddress(String wkaAddress);

        /**
         * Returns the well-known address, this is useful for working out what the default value is.
         *
         * @return WKA address.
         * @since 2.7
         */
        String getWkaAddress();

        /**
         * Sets the well-known address port which is to be used, typically the default value will
         * suffice, but when the need to run with multiple separate autonomous clusters is required,
         * then this value will need to be changed for one of the clusters - otherwise all the
         * cluster members will simply join the same cluster.
         *
         * @param wkaPort Well-known port, e.g. 12345.
         * @return builder.
         */
        Builder setWkaPort(int wkaPort);

        /**
         * Returns the well-known address port, this is useful for working out what the default value
         * is and then subsequently setting it to a different number when running multiple autonomous
         * clusters - since 2.14, it is typically more convenient to get the WKA port from one
         * cluster member group in order to then offset it for another, such as
         * <code>setWkaPort(memberGroup1.getWkaPort() + 100).
         *
         * @return WKA port.
         */
        int getWkaPort();

        /**
         * Sets the Extend port that the Extend proxy should listen on and for which the Extend
         * client should connect to the Extend proxy server.
         *
         * @param extendPort Extend port, e.g. 23451.
         * @return builder.
         */
        Builder setExtendPort(int extendPort);

        /**
         * Returns the Extend proxy port, for working out what the default value is and then
         * subsequently setting it to a different number when running multiple proxy servers
         * - since 2.14, it is typically more convenient to get the Extend port from one
         * cluster member group in order to then offset it for another, such as
         * <code>setExtendPort(memberGroup1.getExtendPort() + 100).
         *
         * @return Extend port.
         */
        int getExtendPort();

        /**
         * Sets the builder properties to be used, these will override any defaults - using
         * properties is useful if the configuration is required to be externalised, rather than
         * the builder being controlled through code.
         *
         * @param properties Properties containing overrides, the keys should match the methods
         *                   exposed on this cluster member group builder interface, minus the
         *                   'set' - so for example to set the WKA port, the entry in the properties
         *                   file would look like WkaPort=345612
         * @return builder.
         */
        Builder setBuilderProperties(Properties properties);

        /**
         * Sets the builder properties to be used, these will override any defaults - using
         * properties is useful if the configuration is required to be externalised, rather than
         * the builder being controlled through code.
         *
         * @param commaDelimitedPropertiesFilenames
         *         Filenames of properties containing overrides, the keys should match the methods
         *         exposed on this cluster member group builder interface, minus the
         *         'set' - so for example to set the WKA port, the entry in the properties
         *         file would look like WkaPort=345612
         * @return builder.
         */
        Builder setBuilderProperties(String commaDelimitedPropertiesFilenames);

        /**
         * Sets the builder properties to be used, these will override any defaults - using
         * properties is useful if the configuration is required to be externalised, rather than
         * the builder being controlled through code.
         *
         * @param propertiesFilenames Filenames of properties containing overrides, the keys should match the methods
         *                            exposed on this cluster member group builder interface, minus the
         *                            'set' - so for example to set the WKA port, the entry in the properties
         *                            file would look like WkaPort=345612
         * @return builder.
         * @since 2.6
         */
        Builder setBuilderProperties(String... propertiesFilenames);

        /**
         * Sets the number of threads to handle starting up the members within a cluster member group.
         *
         * @param numberOfThreadsInStartUpPool Number of threads available to start-up members.
         * @return builder.
         */
        Builder setNumberOfThreadsInStartUpPool(int numberOfThreadsInStartUpPool);

        /**
         * Suggested sleep duration for 3.5.x.
         *
         * @param sleepAfterStopDuration Sleep duration.
         * @return builder.
         */
        Builder setSuggestedSleepAfterStopDuration35x(int sleepAfterStopDuration);

        /**
         * Suggested sleep duration for 3.6.x.
         *
         * @param sleepAfterStopDuration Sleep duration.
         * @return builder.
         */
        Builder setSuggestedSleepAfterStopDuration36x(int sleepAfterStopDuration);

        /**
         * Default suggested sleep duration.
         *
         * @param sleepAfterStopDuration Sleep duration.
         * @return builder.
         */
        Builder setSuggestedSleepAfterStopDurationDefault(int sleepAfterStopDuration);

        /**
         * Sets the duration that Coherence will wait before starting a new cluster, this
         * setting must be used in conjunction with the littlegrid-fast-start-coherence-override.xml
         * which defines the appropriate system property to control the join timeout.
         *
         * @param joinTimeoutMilliseconds Join timeout milliseconds.
         * @return builder.
         */
        Builder setFastStartJoinTimeoutMilliseconds(long joinTimeoutMilliseconds);

        /**
         * Sets a callback handler instance, examples of use could be to add indexes after
         * the cluster member group is started.
         *
         * @param callbackHandlerInstanceClassName
         *         Callback handler instance class name.
         * @return builder.
         * @since 2.6
         */
        Builder setCallbackHandlerInstanceClassName(String callbackHandlerInstanceClassName);

        /**
         * Sets the site name that the cluster members belong to.
         *
         * @param siteName Site name.
         * @return builder.
         * @since 2.8
         */
        Builder setSiteName(String siteName);

        /**
         * Sets the rack name that the cluster members belong to.
         *
         * @param rackName Rack name.
         * @return builder.
         * @since 2.8
         */
        Builder setRackName(String rackName);

        /**
         * Sets the machine name that the cluster members belong to.
         *
         * @param machineName Machine name.
         * @return builder.
         * @since 2.8
         */
        Builder setMachineName(String machineName);
    }

    /**
     * Build exception reporter, reports useful exception information in a form to help with
     * trouble-shooting.
     */
    interface BuildExceptionReporter {
        /**
         * Report on the exception.
         *
         * @param throwable            Throwable.
         * @param builderKeysAndValues Builder keys and values.
         * @param builderKeyToSystemPropertyNameMapping
         *                             Builder key to system property name mapping.
         */
        void report(Throwable throwable,
                    Map<String, String> builderKeysAndValues,
                    Properties builderKeyToSystemPropertyNameMapping);
    }

    /**
     * Callback handler interface, enabling callbacks to be registered for certain lifecycle events.
     *
     * @since 2.6
     */
    interface CallbackHandler {
        /**
         * Performs any necessary setup before cluster member is started.
         */
        void doBeforeStart();

        /**
         * Performs any necessary actions after the cluster member has been started.
         */
        void doAfterStart();

        /**
         * Performs any necessary actions before the cluster member is shutdown.
         */
        void doBeforeShutdown();

        /**
         * Performs any necessary actions after the cluster member has been shutdown.
         */
        void doAfterShutdown();
    }
}
