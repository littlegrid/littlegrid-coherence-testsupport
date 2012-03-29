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

package org.littlegrid.impl;

import org.junit.Test;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupBuildException;
import org.littlegrid.ClusterMemberGroupUtils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Direct starting of cluster members.
 */
public final class DefaultClusterMemberGroupTest {
    @Test(expected = IllegalArgumentException.class)
    public void startWithInvalidNumberOfMembers() {
        DefaultClusterMemberGroup.startClusterMembers(0, null, null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithNullSystemProperties() {
        DefaultClusterMemberGroup.startClusterMembers(1, null, null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithNoSystemProperties() {
        DefaultClusterMemberGroup.startClusterMembers(1, new Properties(), null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithNullClassPath() {
        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(), null, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithNoClassPath() {
        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(), new URL[]{}, null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithNullInstanceClassName()
            throws MalformedURLException {

        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(), getPopulatedUrls(), null, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithEmptyInstanceClassName()
            throws MalformedURLException {

        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(), getPopulatedUrls(), " ", 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void startWithInvalidNumberOfThreads()
            throws MalformedURLException {

        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(), getPopulatedUrls(), "SomeClass", 0);
    }

    @Test(expected = ClusterMemberGroupBuildException.class)
    public void startWhenClassDoesNotExist()
            throws MalformedURLException {

        DefaultClusterMemberGroup.startClusterMembers(1, getPopulatedProperties(),
                getPopulatedUrls(), "SomeClass", 1);
    }

    @Test
    public void shutdownAllRestoreOfSystemProperties()
            throws MalformedURLException {

        final String key = "this-is-a-key-of-a-new-property";

        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newBuilder()
                .buildAndConfigureForNoClient();

        System.setProperty(key, "Adding a new system property");

        assertThat(System.getProperty(key), notNullValue());

        memberGroup.shutdownAll();

        assertThat(System.getProperty(key), nullValue());
    }

    @Test(expected = IllegalArgumentException.class)
    public void constructWithNoCallbackHandler() {
        new DefaultClusterMemberGroup(null, 0, 0, 0);
    }

    @Test
    public void getSleepDurationBasedUponVersion() {
        final int expectedDuration35x = 18;
        final int expectedDuration36x = 17;
        final int expectedDurationDefault = 15;

        final DefaultClusterMemberGroup memberGroup = new DefaultClusterMemberGroup(new DefaultCallbackHandler(),
                expectedDuration35x, expectedDuration36x, expectedDurationDefault);

        assertThat(memberGroup.getSuggestedSleepDurationBasedUponVersion(3.5f), is(expectedDuration35x));
        assertThat(memberGroup.getSuggestedSleepDurationBasedUponVersion(3.6f), is(expectedDuration36x));
        assertThat(memberGroup.getSuggestedSleepDurationBasedUponVersion(3.7f), is(expectedDurationDefault));
    }

    @Test
    public void startAll() {
        final DefaultClusterMemberGroup memberGroup =
                new DefaultClusterMemberGroup(new DefaultCallbackHandler(), 0, 0, 0);

        memberGroup.startAll();
        memberGroup.startAll();
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
