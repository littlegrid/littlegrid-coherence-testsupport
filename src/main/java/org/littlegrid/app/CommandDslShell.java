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

package org.littlegrid.app;

import com.tangosol.net.CacheFactory;
import com.tangosol.util.ClassHelper;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;

/**
 * Command DSL shell.
 *
 * @since 2.15
 */
class CommandDslShell {
    private static final int WAIT_MILLISECONDS_AFTER_STOP_COMMAND = 1000;

    private static final String COMMANDS_ARGUMENT = "commands=";
    private static final String COMMAND_DELIMITER = ";";

    private static final String COMMAND_PROMPT = "lg> ";

    private static final String STOP_MEMBER_COMMAND = "stop member";
    private static final String BYE_COMMAND = "bye";
    private static final String QUIT_COMMAND = "quit";
    private static final String GET_STARTED_MEMBER_IDS_COMMAND = "members";
    private static final String SHUTDOWN_ALL_COMMAND = "shutdown all";
    private static final String STOP_ALL_COMMAND = "stop all";
    private static final String SLEEP_COMMAND = "sleep";
    private static final String SHUTDOWN_MEMBER_COMMAND = "shutdown member";
    private static final String MERGE_STORAGE_ENABLED_COMMAND = "start storage enabled";
    private static final String MERGE_EXTEND_PROXY_COMMAND = "start extend proxy";
    private static final String MERGE_JMX_MONITOR_COMMAND = "start jmx monitor";
    private static final String HELP_COMMAND = "help";
    private static final String COMMENT_COMMAND = "#";
    private static final String CONSOLE_COMMAND = "console";
    private static final String COHQL_COMMAND = "cohql";

    private final InputStream in;
    private final PrintStream out;

    /**
     * Constructor.
     *
     * @param in  Input stream.
     * @param out Output stream.
     */
    public CommandDslShell(final InputStream in,
                           final PrintStream out) {

        this.in = in;
        this.out = out;
    }

    /**
     * Starts the shell to process commands.
     *
     * @param args Commands passed for execution.
     */
    public void start(final String[] args) {
        final ClusterMemberGroup memberGroup = ClusterMemberGroupUtils.newBuilder().buildAndConfigure();

        final String commands = parseCommandsString(args);
        final boolean exit = processCommandsString(memberGroup, commands);

        System.out.println("");

        if (!exit) {
            final Scanner scanner = new Scanner(in);

            processCommandsStream(memberGroup, scanner);
        }

        System.out.println("Exiting");
        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);
    }

    private String parseCommandsString(final String[] args) {
        final String commands = "";

        for (int i = 0; i < args.length; i++) {
            if (args[i].startsWith(COMMANDS_ARGUMENT)) {
                return args[i].replaceAll(COMMANDS_ARGUMENT, "");
            }
        }

        return commands;
    }

    private void processCommandsStream(final ClusterMemberGroup memberGroup,
                                       final Scanner scanner) {

        boolean exit;

        do {
            out.println();
            out.print(COMMAND_PROMPT);

            final String stringEntered = scanner.nextLine();

            exit = processCommandsString(memberGroup, stringEntered);
        } while (!exit);
    }

    private boolean processCommandsString(final ClusterMemberGroup memberGroup,
                                          final String stringEntered) {

        boolean exit = false;
        final String[] commands = stringEntered.split(COMMAND_DELIMITER);

        for (String command : commands) {
            command = command.trim();

            try {
                if (command.startsWith(STOP_MEMBER_COMMAND)) {
                    stopMember(memberGroup, command);

                } else if (command.startsWith(SHUTDOWN_MEMBER_COMMAND)) {
                    shutdownMember(memberGroup, command);

                } else if (command.startsWith(SLEEP_COMMAND)) {
                    sleep(command);

                } else if (command.equals(STOP_ALL_COMMAND)) {
                    stopAll(memberGroup);

                } else if (command.equals(SHUTDOWN_ALL_COMMAND)) {
                    shutdownAll(memberGroup);

                } else if (command.equals(GET_STARTED_MEMBER_IDS_COMMAND)) {
                    outputStartedMemberIds(memberGroup);

                } else if (command.equals(MERGE_STORAGE_ENABLED_COMMAND)) {
                    mergeStorageEnabledMember(memberGroup);

                } else if (command.equals(MERGE_STORAGE_ENABLED_COMMAND + " *")) {
                    mergeStorageEnabledMember(memberGroup, 10);

                } else if (command.startsWith(MERGE_EXTEND_PROXY_COMMAND)) {
                    mergeExtendProxyMember(memberGroup, command);

                } else if (command.startsWith(MERGE_JMX_MONITOR_COMMAND)) {
                    mergeJmxMonitorMember(memberGroup);

                } else if (command.equals(HELP_COMMAND)) {
                    outputHelp();

                } else if (command.equals(BYE_COMMAND) || command.equals(QUIT_COMMAND)) {
                    exit = true;

                } else if (command.startsWith(COMMENT_COMMAND)) {
                    out.println(command);

                } else if (command.equals(CONSOLE_COMMAND)) {
                    console();

                } else if (command.equals(COHQL_COMMAND)) {
                    cohQl();

                } else if (command.equals("")) {
                    System.out.println("HHHHHHHHHHHHHHHHHHHHHHHHH");
                    // Do nothing

                } else {
                    out.println(format("'%s' is an unknown command", command));
                }
            } catch (Exception e) {
                out.println(format("Exception when executing command: '%s' due to: %s", command, e));
            }
        }

        return exit;
    }

    @SuppressWarnings("unchecked")
    private void console()
            throws Exception {

        CacheFactory.main(new String[]{});

        Class coherenceConsole = Class.forName("com.tangosol.coherence.component.application.console.Coherence",
                true, Thread.currentThread().getContextClassLoader());

        Object component = ClassHelper.invokeStatic(coherenceConsole, "get_Instance", null);
        Method stopMethod = coherenceConsole.getDeclaredMethod("setStop", boolean.class);

        stopMethod.setAccessible(true);
        stopMethod.invoke(component, false);
    }

    private void cohQl()
            throws Exception {

        final Class cohqlConsole =
                this.getClass().getClassLoader().loadClass("com.tangosol.coherence.dslquery.QueryPlus");

        ClassHelper.invokeStatic(cohqlConsole, "main", new Object[]{new String[]{}});
    }

    private void outputHelp() {
        out.println(format("%s n m - stops the specified cluster member(s)", STOP_MEMBER_COMMAND));
        out.println(format("%s n m - shuts down the specified cluster member(s)", SHUTDOWN_MEMBER_COMMAND));
        out.println(format("%s - stops all cluster member(s)", STOP_ALL_COMMAND));
        out.println(format("%s - shuts down all cluster member(s)", SHUTDOWN_ALL_COMMAND));
        out.println(format("%s - exits this application - same as %s", BYE_COMMAND, QUIT_COMMAND));
        out.println(format("%s - quits this application - same as %s", QUIT_COMMAND, BYE_COMMAND));
        out.println(format("%s - displays member Ids known to this process", GET_STARTED_MEMBER_IDS_COMMAND));
        out.println(format("%s n - sleeps for the specified time in milliseconds", SLEEP_COMMAND));
        out.println(format("%s - starts a storage enabled member in this process", MERGE_STORAGE_ENABLED_COMMAND));
        out.println(format("%s n - starts an Extend proxy member with specified port in this process",
                MERGE_EXTEND_PROXY_COMMAND));
        out.println(format("%s - starts a JMX monitor member in this process", MERGE_JMX_MONITOR_COMMAND));
        out.println(format("%s - displays this help", HELP_COMMAND));
        out.println(format("%s - a comment line, useful when scripting and wanting to comment scripts",
                COMMENT_COMMAND));
        out.println(format("%s - launches CohQL console", COHQL_COMMAND));
        out.println(format("%s - launches Coherence console (not for Extend clients)", CONSOLE_COMMAND));
    }

    private void mergeJmxMonitorMember(final ClusterMemberGroup memberGroup) {
        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(0)
                .setExtendProxyCount(0)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(1)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds(memberGroup);
    }

    private void mergeExtendProxyMember(final ClusterMemberGroup memberGroup,
                                        final String command) {

        final int extendPort = parseInteger(MERGE_EXTEND_PROXY_COMMAND, command);

        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(0)
                .setExtendProxyCount(1)
                .setExtendPort(extendPort)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds(memberGroup);
    }

    private void mergeStorageEnabledMember(final ClusterMemberGroup memberGroup) {
        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1)
                .setExtendProxyCount(0)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds(memberGroup);
    }

    private void mergeStorageEnabledMember(final ClusterMemberGroup memberGroup,
                                           final int numberOfMembers) {

        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .setExtendProxyCount(0)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds(memberGroup);
    }

    private void shutdownAll(final ClusterMemberGroup memberGroup) {
        memberGroup.shutdownAll();
    }

    private void stopAll(final ClusterMemberGroup memberGroup) {
        memberGroup.stopAll();
    }

    private void sleep(final String command)
            throws InterruptedException {

        final int sleepTime = parseInteger(SLEEP_COMMAND, command);

        out.println(format("About to sleep for %d milliseconds", sleepTime));
        TimeUnit.MILLISECONDS.sleep(sleepTime);
    }

    private void shutdownMember(final ClusterMemberGroup memberGroup,
                                final String command) {

        final int[] memberId = parseIntegers(SHUTDOWN_MEMBER_COMMAND, command);

        memberGroup.shutdownMember(memberId);

        outputStartedMemberIds(memberGroup);
    }

    private void stopMember(final ClusterMemberGroup memberGroup,
                            final String command)
            throws InterruptedException {

        final int[] memberId = parseIntegers(STOP_MEMBER_COMMAND, command);

        memberGroup.stopMember(memberId);

        TimeUnit.MILLISECONDS.sleep(WAIT_MILLISECONDS_AFTER_STOP_COMMAND);

        outputStartedMemberIds(memberGroup);
    }

    private void outputStartedMemberIds(final ClusterMemberGroup memberGroup) {
        out.println("Started member ids: " + Arrays.toString(memberGroup.getStartedMemberIds()));
    }

    private static int parseInteger(final String command,
                                    final String commandAndNumber) {

        return parseIntegers(command, commandAndNumber)[0];
    }

    private static int[] parseIntegers(final String command,
                                       final String commandAndCommaDelimitedNumbers) {

        final String commaDelimitedNumbers = commandAndCommaDelimitedNumbers.replaceAll(command, "");
        final String[] stringNumbers = commaDelimitedNumbers.trim().split(" ");

        final int[] numbers = new int[stringNumbers.length];

        for (int i = 0; i < numbers.length; i++) {
            final String value = stringNumbers[i].trim();

            if (value.length() > 0) {
                numbers[i] = Integer.parseInt(stringNumbers[i].trim());
            }
        }

        return numbers;
    }
}
