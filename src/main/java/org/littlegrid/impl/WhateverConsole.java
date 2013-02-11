package org.littlegrid.impl;

import com.tangosol.coherence.component.util.logOutput.Log4j;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.ClusterMemberGroupAware;

/**
 */
public class WhateverConsole implements ClusterMemberGroupAware {
    private static final String COMMAND_PROMPT = "lg> ";
    private static final String STOP_MEMBER_COMMAND = "stopMember";
    private static final String BYE_COMMAND = "bye";
    private static final String GET_STARTED_MEMBER_IDS_COMMAND = "getStartedMemberIds";
    private static final String SHUTDOWN_ALL_COMMAND = "shutdownAll";
    private static final String STOP_ALL_COMMAND = "stopAll";
    private static final String SLEEP_COMMAND = "sleep";
    private static final String SHUTDOWN_MEMBER_COMMAND = "shutdownMember";
    private static final String MERGE_STORAGE_ENABLED_COMMAND = "mergeStorageEnabledMember";
    private static final String MERGE_EXTEND_PROXY_COMMAND = "mergeExtendProxyMember";
    private static final String HELP_COMMAND = "help";
    private static final String PAUSE_COMMAND = "pause";
    private static final String COMMENT_COMMAND = "#";

    private ClusterMemberGroup memberGroup;

    private PrintStream out = System.out;

    public void main(String[] args)
            throws IOException, InterruptedException {

        final Scanner scanner = new Scanner(System.in);
//        final Scanner scanner = new Scanner(new FileInputStream("whatever.txt"));
        boolean exit = false;

        do {
            out.println();
            out.print(COMMAND_PROMPT);
            final String stringEntered = scanner.nextLine();
            final String[] commands = stringEntered.split(";");

            for (String command : commands) {
                try {
                    command = command.trim();

                    if (command.startsWith(STOP_MEMBER_COMMAND)) {
                        stopMember(command);

                    } else if (command.startsWith(SHUTDOWN_MEMBER_COMMAND)) {
                        shutdownMember(command);

                    } else if (command.startsWith(SLEEP_COMMAND)) {
                        sleep(command);

                    } else if (command.equals(STOP_ALL_COMMAND)) {
                        stopAll();

                    } else if (command.equals(SHUTDOWN_ALL_COMMAND)) {
                        shutdownAll();

                    } else if (command.equals(GET_STARTED_MEMBER_IDS_COMMAND)) {
                        outputStartedMemberIds();

                    } else if (command.equals(MERGE_STORAGE_ENABLED_COMMAND)) {
                        mergeStorageEnabledMember();

                    } else if (command.startsWith(MERGE_EXTEND_PROXY_COMMAND)) {
                        mergeExtendProxyMember(command);

                    } else if (command.equals(HELP_COMMAND)) {
                        outputHelp();

                    } else if (command.equals(PAUSE_COMMAND)) {
                        pause();

                    } else if (command.equals(BYE_COMMAND)) {
                        exit = true;

                    } else if (command.startsWith(COMMENT_COMMAND)) {
                        out.println(command);

                    } else if (command.equals("")) {
                        // Do nothing

                    } else {
                        out.println(format("'%s' is an unknown command", command));
                    }
                } catch (Exception e) {
                    out.println(format("Exception when executing command: '%s' due to: %s", command, e));
                }
            }
        } while (!exit);
    }

    private void pause()
            throws IOException {

        out.println("Pausing, press <Enter> to continue");

        System.in.read();
    }

    private void outputHelp() {
        out.println(format("%s n, m - stops the specified cluster member(s)", STOP_MEMBER_COMMAND));
        out.println(format("%s n, m - shuts down the specified cluster member(s)", SHUTDOWN_MEMBER_COMMAND));
        out.println(format("%s - stops all cluster member(s)", STOP_ALL_COMMAND));
        out.println(format("%s - shuts down all cluster member(s)", SHUTDOWN_ALL_COMMAND));
        out.println(format("%s - exits this application", BYE_COMMAND));
        out.println(format("%s - displays member Ids known to this application", GET_STARTED_MEMBER_IDS_COMMAND));
        out.println(format("%s n - sleeps for the specified time in seconds", SLEEP_COMMAND));
        out.println(format("%s - merges storage enabled member into this application", MERGE_STORAGE_ENABLED_COMMAND));
        out.println(format("%s n - merges Extend proxy member with specified port into this application", MERGE_EXTEND_PROXY_COMMAND));
        out.println(format("%s - displays this help", HELP_COMMAND));
        out.println(format("%s - pauses until Enter key is pressed", PAUSE_COMMAND));
        out.println(format("%s - a comment line, useful when scripting", COMMENT_COMMAND));
    }

    private void mergeExtendProxyMember(final String command) {
        final int extendPort = parseInteger(MERGE_EXTEND_PROXY_COMMAND, command);

        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(0)
                .setExtendProxyCount(1)
                .setExtendPort(extendPort)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds();
    }

    private void mergeStorageEnabledMember() {
        memberGroup.merge(ClusterMemberGroupUtils.newBuilder()
                .setStorageEnabledCount(1)
                .setExtendProxyCount(0)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0)
                .buildAndConfigure());

        outputStartedMemberIds();
    }

    private void shutdownAll() {
        memberGroup.shutdownAll();
    }

    private void stopAll() {
        memberGroup.stopAll();
    }

    private void sleep(final String command) throws InterruptedException {
        final int sleepTime = parseInteger(SLEEP_COMMAND, command);

        out.println(format("About to sleep for %d seconds", sleepTime));
        TimeUnit.SECONDS.sleep(sleepTime);
    }

    private void shutdownMember(final String command) {
        final int[] memberId = parseIntegers(SHUTDOWN_MEMBER_COMMAND, command);

        memberGroup.shutdownMember(memberId);

        outputStartedMemberIds();
    }

    private void stopMember(final String command) throws InterruptedException {
        final int[] memberId = parseIntegers(STOP_MEMBER_COMMAND, command);

        memberGroup.stopMember(memberId);

        TimeUnit.MILLISECONDS.sleep(1250);

        outputStartedMemberIds();
    }

    private void outputStartedMemberIds() {
        out.println("Started member ids: " + Arrays.toString(memberGroup.getStartedMemberIds()));
    }

    private static int parseInteger(final String command,
                                    final String commandAndNumber) {

        return parseIntegers(command, commandAndNumber)[0];
    }

    private static int[] parseIntegers(final String command,
                                       final String commandAndCommaDelimitedNumbers) {

        final String commaDelimitedNumbers = commandAndCommaDelimitedNumbers.replaceAll(command, "");
        final String[] stringNumbers = commaDelimitedNumbers.split(",");

        final int[] numbers = new int[stringNumbers.length];

        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = Integer.parseInt(stringNumbers[i].trim());
        }

        return numbers;
    }

    @Override
    public void setClusterMemberGroup(final ClusterMemberGroup clusterMemberGroup) {
        memberGroup = clusterMemberGroup;
    }
}
