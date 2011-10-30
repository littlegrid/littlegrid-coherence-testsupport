package org.jhall.coherence.testsupport.server.impl;

import org.apache.log4j.Logger;
import org.jhall.coherence.testsupport.server.ClusterMemberGroup;
import org.jhall.coherence.testsupport.server.ClusterMemberGroupConfig;
import org.jhall.coherence.testsupport.server.ClusterMemberGroupRuntimeException;
import org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus;
import org.jhall.coherence.testsupport.server.impl.util.PropertyContainer;

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
import static org.jhall.coherence.testsupport.SystemPropertyConst.TANGOSOL_COHERENCE_DOT;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.HALTED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.HALTING;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.INITIALIZATION_PROBLEM;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.NEVER_STARTED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.RUNNING;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.SHUTDOWN_FAILED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.STARTING;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.START_FAILED;
import static org.jhall.coherence.testsupport.server.ClusterMemberGroupStatus.STOP_FAILED;
import static org.jhall.coherence.testsupport.server.impl.util.SystemUtils.getSystemPropertiesWithPrefix;
import static org.jhall.coherence.testsupport.server.impl.util.SystemUtils.setReplaceClearSystemProperties;

/**
 * Default local process cluster member group implementation.
 */
public class DefaultLocalProcessClusterMemberGroupImpl implements ClusterMemberGroup {
    private ClusterMemberGroupStatus status = NEVER_STARTED;
    private Logger logger = Logger.getLogger(this.getClass());
    private ClusterMemberGroupConfig groupConfig;
    private PropertyContainer propertyContainer;
    private List<Future<DelegatingClusterMemberWrapper>> futuresOfMemberWrapper;

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
                logger.debug("Cluster member group config class path URLs null, setting to current (minus Java home)");

                groupConfig.setClassPathUrls(getClassPathUrlsExcludingJavaHome(groupConfig.getJarsToExcludeFromClassPathUrls()));
            }
        } catch (Exception e) {
            status = INITIALIZATION_PROBLEM;

            throw new ClusterMemberGroupRuntimeException(e);
        }
    }

    /**
     * Returns current status.
     *
     * @return status.
     */
    public ClusterMemberGroupStatus getStatus() {
        return status;
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
                            logger.debug(format("JAR: '%s' specified for exclusion from class path", jarToExclude));
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
    public void startAll() {
        if (status != NEVER_STARTED) {
            return;
        }

        status = STARTING;
        outputStartAllMessages();

        final int numberOfClusterMembers = groupConfig.getNumberOfClusterMembers();

        //TODO: Look to 'isolate' these properties
        PropertyContainer replacedSystemProperties = setReplaceClearSystemProperties(propertyContainer);

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

            logger.debug("About to establish a cluster using a single member initially");
            Future<DelegatingClusterMemberWrapper> futureForSeniorMember = executorService.submit(taskForSeniorMember);
            futureForSeniorMember.get();

            logger.debug("First cluster member up, starting any remaining members to join cluster via established WKA");
            futuresOfMemberWrapper = executorService.invokeAll(tasks);
            futuresOfMemberWrapper.add(futureForSeniorMember);
            executorService.shutdown();

            List<Integer> memberIds = getStartedMemberIds();

            logger.info(format("Group of cluster member(s) started, member Ids: %s", memberIds));
        } catch (Exception e) {
            status = START_FAILED;
            String message = format(
                    "Failed to start cluster member group - check Coherence system properties for misconfiguration: %s",
                    getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT));

            logger.error(message);
            throw new ClusterMemberGroupRuntimeException(message, e);
        } finally {
            setReplaceClearSystemProperties(replacedSystemProperties);
        }

        status = RUNNING;
    }

    private void outputStartAllMessages() {
        final int oneMB = 1024 * 1024;

        logger.info(format("About to start '%d' cluster member(s) in group, using '%d' threads in pool",
                groupConfig.getNumberOfClusterMembers(), groupConfig.getNumberOfThreadsInStartUpPool()));

        logger.debug(format("Class path (after exclusions)..: %s", Arrays.deepToString(groupConfig.getClassPathUrls())));
        logger.debug(format("Current Coherence properties...: %s", getSystemPropertiesWithPrefix(TANGOSOL_COHERENCE_DOT)));
        logger.info(format("Server system properties to set: %s", propertyContainer));
        logger.debug(format("Max memory: %sMB, current: %sMB, free memory: %sMB",
                Runtime.getRuntime().maxMemory() / oneMB,
                Runtime.getRuntime().totalMemory() / oneMB,
                Runtime.getRuntime().freeMemory() / oneMB));
    }

    private DelegatingClusterMemberWrapper getClusterMemberWrapper(int memberId) {
        if (status != RUNNING) {
            return null;
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
            throw new ClusterMemberGroupRuntimeException(e);
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
            throw new ClusterMemberGroupRuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shutdown(int memberId) {
        if (status != RUNNING) {
            return false;
        }

        logger.info(String.format("About to shutdown cluster member '%d'", memberId));

        DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);
        boolean shutdown = false;

        if (memberWrapper != null) {
            memberWrapper.shutdown();
            shutdown = true;
        }

        return shutdown;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void shutdownAll() {
        if (status != RUNNING) {
            return;
        }

        status = HALTING;
        logger.info(format("Shutting down '%d' cluster member(s) in group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.shutdown();
            }

            futuresOfMemberWrapper.clear();

            logger.info("Group of cluster member(s) shutdown");
        } catch (Exception e) {
            status = SHUTDOWN_FAILED;

            throw new ClusterMemberGroupRuntimeException(e);
        }

        status = HALTED;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean stop(int memberId) {
        if (status != RUNNING) {
            return false;
        }

        logger.info(String.format("About to stop cluster member '%d'", memberId));

        DelegatingClusterMemberWrapper memberWrapper = getClusterMemberWrapper(memberId);
        boolean stopped = false;

        if (memberWrapper != null) {
            memberWrapper.stop();
            stopped = true;
        }

        return stopped;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean[] stop(Integer[] memberId) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stopAll() {
        if (status != RUNNING) {
            return;
        }

        status = HALTING;
        logger.info(format("Stopping '%d' cluster member(s) in this group", groupConfig.getNumberOfClusterMembers()));

        try {
            for (int i = 0; i < futuresOfMemberWrapper.size(); i++) {
                Future<DelegatingClusterMemberWrapper> task = futuresOfMemberWrapper.get(i);

                DelegatingClusterMemberWrapper memberWrapper = task.get();
                memberWrapper.stop();
            }

            status = HALTED;
        } catch (Exception e) {
            status = STOP_FAILED;

            throw new ClusterMemberGroupRuntimeException(e);
        }
    }
}
