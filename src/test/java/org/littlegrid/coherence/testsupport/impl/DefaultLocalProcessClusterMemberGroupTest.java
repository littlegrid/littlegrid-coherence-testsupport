package org.littlegrid.coherence.testsupport.impl;

import org.junit.Ignore;
import org.junit.Test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

/**
 * Direct (i.e. not going through ClusterMemberGroupUtils) default local process cluster member
 * group tests.
 */
public class DefaultLocalProcessClusterMemberGroupTest {
    @Test(expected = IllegalArgumentException.class)
    public void constructWithInvalidNumberOfMembers() {
        new DefaultLocalProcessClusterMemberGroup(0, null, null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNullSystemProperties() {
        new DefaultLocalProcessClusterMemberGroup(1, null, null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNoSystemProperties() {
        new DefaultLocalProcessClusterMemberGroup(1, new Properties(), null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNullClassPath() {
        new DefaultLocalProcessClusterMemberGroup(1, getPopulatedProperties(), null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNoClassPath() {
        new DefaultLocalProcessClusterMemberGroup(1, getPopulatedProperties(), new URL[]{}, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNullInstanceClassName()
            throws MalformedURLException {

        new DefaultLocalProcessClusterMemberGroup(1, getPopulatedProperties(), getPopulatedUrls(), null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithEmptyInstanceClassName()
            throws MalformedURLException {

        new DefaultLocalProcessClusterMemberGroup(1, getPopulatedProperties(), getPopulatedUrls(), " ", 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithInvalidNumberOfThreads()
            throws MalformedURLException {

        new DefaultLocalProcessClusterMemberGroup(1, getPopulatedProperties(), getPopulatedUrls(), "SomeClass", 0);
    }

    @Test
    @Ignore
    public void shutdownAllRestoreOfSystemProperties() {
        Properties propertiesBefore = SystemUtils.snapshotSystemProperties();

        System.setProperty("key", "Adding a new system property");
        throw new UnsupportedOperationException();
    }

    private static Properties getPopulatedProperties() {
        Properties properties = new Properties();
        properties.setProperty("key", "value");

        return properties;
    }

    private static URL[] getPopulatedUrls()
            throws MalformedURLException {

        return new URL[]{new URL("file://url")};
    }
}
