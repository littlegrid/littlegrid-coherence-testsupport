package org.testsupport.coherence.impl;

import org.testsupport.coherence.ClusterMember;
import org.testsupport.coherence.ClusterMemberGroup;
import org.testsupport.common.lang.PropertyContainer;
import org.testsupport.common.lang.SystemUtils;

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
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.testsupport.coherence.CoherenceSystemPropertyConst.TANGOSOL_COHERENCE_DOT;

/**
 * Default local process cluster member group implementation.
 */
public class DefaultLocalProcessClusterMemberGroupImpl implements ClusterMemberGroup {
    private final Logger logger = Logger.getLogger(DefaultLocalProcessClusterMemberGroupImpl.class.getName());
    private boolean startInvoked;
    private ClusterMemberGroupConfig groupConfig;
    private PropertyContainer propertyContainer;
    private List<Future<DelegatingClusterMemberWrapper>> memberFutures;


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
                logger.fine("Cluster member group config class path URLs null, setting to current (minus Java home)");

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
                            logger.fine(format("JAR: '%s' specified for exclusion from class path", jarToExclude));

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

            logger.fine("About to establish a cluster using a single member initially");
            Future<DelegatingClusterMemberWrapper> futureForSeniorMember = executorService.submit(taskForSeniorMember);
            futureForSeniorMember.get();

            logger.info("First cluster member up, starting any remaining members to join established cluster");
            memberFutures = executorService.invokeAll(tasks);
            memberFutures.add(futureForSeniorMember);
            executorService.shutdown();

            List<Integer> memberIds = getStartedMemberIds();

            logger.info(format("Group of cluster member(s) started, member Ids: %s", memberIds));
        } catch (Exception e) {
            String message = format(
                    "Failed to start cluster member group - check Coherence system properties for misconfiguration: %s",
                    SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));

            logger.severe(message);
            throw new IllegalStateException(message, e);
        } finally {
            SystemUtils.setReplaceClearSystemProperties(replacedSystemProperties);
        }

        return this;
    }

    private void outputStartAllMessages() {
        final int oneMB = 1024 * 1024;

        logger.info(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
                groupConfig.getNumberOfClusterMembers(), groupConfig.getNumberOfThreadsInStartUpPool()));

        logger.fine(format("Class path (after exclusions)..: %s", Arrays.deepToString(groupConfig.getClassPathUrls())));
        logger.fine(format("Current Coherence properties...: %s", SystemUtils.getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));
        logger.info(format("Server system properties to set: %s", propertyContainer));
        logger.fine(format("Max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private DelegatingClusterMemberWrapper getClusterMemberWrapper(int memberId) {
        if (!startInvoked) {
            throw new IllegalStateException("Cluster member group never started");
        }

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = memberFutures.get(i);

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

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Integer> getStartedMemberIds() {
        try {
            List<Integer> memberIds = new ArrayList<Integer>();

            for (int i = 0; i < memberFutures.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = memberFutures.get(i);

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
    public ClusterMember getClusterMember(int memberId) {
        if (!startInvoked) {
            logger.warning(format("Cluster member group never started - cannot get member '%s'", memberId));

            return null;
        }

        logger.fine(format("About to get cluster member '%d'", memberId));

        return getClusterMemberWrapper(memberId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup shutdownMember(int... memberIds) {
        if (!startInvoked) {
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Shutting down multiple members is not supported currently");
        }

        int memberId = memberIds[0];

        logger.info(format("About to shutdown cluster member '%d'", memberId));

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
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }

        logger.info(format("Shutting down '%d' cluster member(s) in group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = memberFutures.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.shutdown();
            }

            memberFutures.clear();

            logger.info("Group of cluster member(s) shutdown");
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        //TODO: reset system properties after shutdown
        logger.warning("RESET THE SYSTEM PROPERTIES AFTER THE SHUTDOWN");
        logger.warning("RESET THE SYSTEM PROPERTIES AFTER THE SHUTDOWN");
        logger.warning("RESET THE SYSTEM PROPERTIES AFTER THE SHUTDOWN");

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ClusterMemberGroup stopMember(int... memberIds) {
        if (!startInvoked) {
            logger.warning("Cluster member group never started - nothing to ");

            return this;
        }

        if (memberIds.length > 1) {
            throw new UnsupportedOperationException("Stopping multiple members is not supported currently");
        }

        int memberId = memberIds[0];

        logger.info(format("About to stop cluster member '%d'", memberId));

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
            logger.warning("Cluster member group never started - nothing to shutdown");

            return this;
        }

        logger.info(format("Stopping '%d' cluster member(s) in this group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < memberFutures.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = memberFutures.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.stop();
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }

        return this;
    }
}
