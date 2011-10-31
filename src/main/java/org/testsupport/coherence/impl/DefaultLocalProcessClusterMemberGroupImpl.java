package org.testsupport.coherence.impl;

import com.tangosol.net.CacheFactory;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.util.SystemUtils;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static java.lang.String.format;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT;

/**
 * Default local process cluster member group implementation.
 */
public class DefaultLocalProcessClusterMemberGroupImpl implements ClusterMemberGroup {
    private boolean startInvoked;
    //    private Logger logger = Logger.getLogger(this.getClass());
    private ClusterMemberGroupConfig groupConfig;
    private PropertyContainer propertyContainer;
    private List<Future<DelegatingClusterMemberWrapper>> futuresOfMemberWrapper;

    protected void logDebug(final String message,
                            final Object... args) {

        throw new UnsupportedOperationException();
    }

    protected void logWarn(final String message,
                           final Object... args) {

        CacheFactory.log(format(message, args), CacheFactory.LOG_WARN);
    }

    public DefaultLocalProcessClusterMemberGroupImpl(Builder builder) {

    }

    /**
     * Constructor.
     *
     * @param propertyContainer Property container.
     * @param groupConfig       Cluster member group config.
     */
    public DefaultLocalProcessClusterMemberGroupImpl(PropertyContainer propertyContainer,
                                                     ClusterMemberGroupConfig groupConfig) {

        if (propertyContainer == null) {
            throw new IllegalStateException("Property container cannot be null");
        }

        if (groupConfig == null) {
            throw new IllegalStateException("Cluster member group configuration cannot be null");
        }

        this.propertyContainer = propertyContainer;
        this.groupConfig = groupConfig;

        try {
            if (groupConfig.getClassPathUrls() == null) {
//                logger.debug("Cluster member group config class path URLs null, setting to current (minus Java home)");
                CacheFactory.log("Cluster member group config class path URLs null, setting to current (minus Java home)");

                groupConfig.setClassPathUrls(getClassPathUrlsExcludingJavaHome(groupConfig.getJarsToExcludeFromClassPathUrls()));
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    private URL[] getClassPathUrlsExcludingJavaHome(String... jarsToExcludeFromClassPathUrls)
            throws MalformedURLException {

        //TODO: Pull out the JAR exclusion code if this feature seems like it will be required
        String pathSeparator = System.getProperty("path.separator");
        String[] classPathArray = System.getProperty("java.class.path").split(pathSeparator);
        String javaHome = System.getProperty("java.home");

        List<URL> classPathUrls = new ArrayList<URL>();

        for (String partOfClassPath : classPathArray) {
            if (!partOfClassPath.startsWith(javaHome)) {
                boolean found = false;

                if (jarsToExcludeFromClassPathUrls != null) {
                    for (String jarToExclude : jarsToExcludeFromClassPathUrls) {
                        if (partOfClassPath.endsWith(jarToExclude)) {
//                            logger.debug(format("JAR: '%s' specified for exclusion from class path", jarToExclude));
                            CacheFactory.log(format("JAR: '%s' specified for exclusion from class path", jarToExclude));
                            found = true;
                        }
                    }
                }

                if (!found) {
                    classPathUrls.add(new File(partOfClassPath).toURI().toURL());
                }
            }
        }

        return classPathUrls.toArray(new URL[classPathUrls.size()]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup startAll() {
        if (startInvoked) {
            return this;
        }

        startInvoked = true;
        outputStartAllMessages();

        final int numberOfClusterMembers = groupConfig.getNumberOfClusterMembers();

        //TODO: Look to 'isolate' these properties
        PropertyContainer replacedSystemProperties = SystemUtils.setReplaceClearSystemProperties(propertyContainer);

        try {
            List<Callable<DelegatingClusterMemberWrapper>> tasks =
                    new ArrayList<Callable<DelegatingClusterMemberWrapper>>(numberOfClusterMembers);

            for (int i = 0; i < numberOfClusterMembers; i++) {
                tasks.add(new ClusterMemberCallable(groupConfig.getClusterMemberClassName(),
                        groupConfig.getClassPathUrls()));
            }

            Callable<DelegatingClusterMemberWrapper> taskForSeniorMember = tasks.remove(0);

            ExecutorService executorService =
                    Executors.newFixedThreadPool(groupConfig.getNumberOfThreadsInStartUpPool());

//            logger.debug("About to establish a cluster using a single member initially");
            CacheFactory.log("About to establish a cluster using a single member initially");
            Future<DelegatingClusterMemberWrapper> futureForSeniorMember = executorService.submit(taskForSeniorMember);
            futureForSeniorMember.get();

//            logger.debug("First cluster member up, starting any remaining members to join cluster via established WKA");
            CacheFactory.log("First cluster member up, starting any remaining members to join cluster via established WKA");
            futuresOfMemberWrapper = executorService.invokeAll(tasks);
            futuresOfMemberWrapper.add(futureForSeniorMember);
            executorService.shutdown();

            List<Integer> memberIds = getStartedMemberIds();

//            logger.info(format("Group of cluster member(s) started, member Ids: %s", memberIds));
            CacheFactory.log(format("Group of cluster member(s) started, member Ids: %s", memberIds));
        } catch (Exception e) {
            String message = String.format(
                    "Failed to start cluster member group - check Coherence system properties for misconfiguration: %s",
                    SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));

//            logger.error(message);
            CacheFactory.log(message);
            throw new IllegalStateException(message, e);
        } finally {
            SystemUtils.setReplaceClearSystemProperties(replacedSystemProperties);
        }

        return this;
    }

    private void outputStartAllMessages() {
        final int oneMB = 1024 * 1024;

//        logger.info(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
//                groupConfig.getNumberOfClusterMembers(), groupConfig.getNumberOfThreadsInStartUpPool()));
//
//        logger.debug(format("Class path (after exclusions)..: %s", Arrays.deepToString(groupConfig.getClassPathUrls())));
//        logger.debug(String.format("Current Coherence properties...: %s", SystemUtils.getSystemPropertiesWithPrefix(CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT)));
//        logger.info(format("Server system properties to set: %s", propertyContainer));
//        logger.debug(format("Max memory: %sMB, current: %sMB, free memory: %sMB",
//                Runtime.getRuntime().maxMemory() / oneMB,
//                Runtime.getRuntime().totalMemory() / oneMB,
//                Runtime.getRuntime().freeMemory() / oneMB));
        CacheFactory.log(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
                groupConfig.getNumberOfClusterMembers(), groupConfig.getNumberOfThreadsInStartUpPool()));

        CacheFactory.log(format("Class path (after exclusions)..: %s", Arrays.deepToString(groupConfig.getClassPathUrls())));
        CacheFactory.log(String.format("Current Coherence properties...: %s", SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));
        CacheFactory.log(format("Server system properties to set: %s", propertyContainer));
        CacheFactory.log(format("Max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private DelegatingClusterMemberWrapper getClusterMemberWrapper(int memberId) {
        if (!startInvoked) {
            throw new IllegalStateException("Cluster member group never started");
        }

        try {
            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();

                if (memberWrapper.getLocalMemberId() == memberId) {
                    return memberWrapper;
                }
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return null;
    }

    private List<Integer> getStartedMemberIds() {
        try {
            List<Integer> memberIds = new ArrayList<Integer>();

            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberIds.add(memberWrapper.getLocalMemberId());
            }

            return memberIds;
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownMember(int memberId) {
        if (!startInvoked) {
            logWarn("Cluster member group never started - member '%s' not running to shutdown", memberId);

            return this;
        }

//        logger.info(String.format("About to shutdown cluster member '%d'", memberId));
        CacheFactory.log(String.format("About to shutdown cluster member '%d'", memberId));

        DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);

        if (memberWrapper != null) {
            memberWrapper.shutdown();
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownAll() {
        if (!startInvoked) {
            logWarn("Cluster member group never started - nothing to shutdown");

            return this;
        }

//        logger.info(format("Shutting down '%d' cluster member(s) in group", groupConfig.getNumberOfClusterMembers()));
        CacheFactory.log(format("Shutting down '%d' cluster member(s) in group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.shutdown();
            }

            futuresOfMemberWrapper.clear();

//            logger.info("Group of cluster member(s) shutdown");
            CacheFactory.log("Group of cluster member(s) shutdown");
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopMember(int memberId) {
        if (!startInvoked) {
            logWarn("Cluster member group never started - member '%s' not running to stop", memberId);

            return this;
        }

//        logger.info(String.format("About to stop cluster member '%d'", memberId));
        CacheFactory.log(String.format("About to stop cluster member '%d'", memberId));

        DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);

        if (memberWrapper != null) {
            memberWrapper.stop();
        }

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopAll() {
        if (!startInvoked) {
            logWarn("Cluster member group never started - nothing to shutdown");

            return this;
        }

//        logger.info(format("Stopping '%d' cluster member(s) in this group", groupConfig.getNumberOfClusterMembers()));
        CacheFactory.log(format("Stopping '%d' cluster member(s) in this group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.stop();
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }
}
