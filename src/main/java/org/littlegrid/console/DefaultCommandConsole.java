package org.littlegrid.console;

import com.tangosol.coherence.dslquery.QueryPlus;
import org.littlegrid.ClusterMemberGroup;
import org.littlegrid.ClusterMemberGroupUtils;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.Console;

/**
 */
public class DefaultCommandConsole implements Console {
    private static final String COMMAND_PROMPT = "lg> ";
    private static final String STOP_MEMBER_COMMAND = "stop member";
    private static final String BYE_COMMAND = "bye";
    private static final String QUIT_COMMAND = "quit";
    private static final String GET_STARTED_MEMBER_IDS_COMMAND = "get started";
    private static final String SHUTDOWN_ALL_COMMAND = "shutdown all";
    private static final String STOP_ALL_COMMAND = "stop all";
    private static final String SLEEP_COMMAND = "sleep";
    private static final String SHUTDOWN_MEMBER_COMMAND = "shutdown member";
    private static final String MERGE_STORAGE_ENABLED_COMMAND = "merge storage enabled";
    private static final String MERGE_EXTEND_PROXY_COMMAND = "merge extend proxy";
    private static final String HELP_COMMAND = "help";
    private static final String COMMENT_COMMAND = "#";
    private static final String COHQL_COMMAND = "cohql";
    private static final int WAIT_MILLISECONDS_AFTER_STOP_COMMAND = 1250;

    private InputStream in;
    private PrintStream out;

    @Override
    public void initialiseStreams(final String[] args) {
        setInputStream(System.in);
        setPrintStream(System.out);
    }

    public void setInputStream(final InputStream in) {
        this.in = in;
    }

    public void setPrintStream(final PrintStream out) {
        this.out = out;
    }

    @Override
    public ClusterMemberGroup build(final ClusterMemberGroup.Builder builder) {
        return builder.buildAndConfigureForNoClient();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void start(ClusterMemberGroup clusterMemberGroup) {
        final Scanner scanner = new Scanner(in);
//        final Scanner scanner = new Scanner(System.in);
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
                        stopMember(clusterMemberGroup, command);

                    } else if (command.startsWith(SHUTDOWN_MEMBER_COMMAND)) {
                        shutdownMember(clusterMemberGroup, command);

                    } else if (command.startsWith(SLEEP_COMMAND)) {
                        sleep(command);

                    } else if (command.equals(STOP_ALL_COMMAND)) {
                        stopAll(clusterMemberGroup);

                    } else if (command.equals(SHUTDOWN_ALL_COMMAND)) {
                        shutdownAll(clusterMemberGroup);

                    } else if (command.equals(GET_STARTED_MEMBER_IDS_COMMAND)) {
                        outputStartedMemberIds(clusterMemberGroup);

                    } else if (command.equals(MERGE_STORAGE_ENABLED_COMMAND)) {
                        mergeStorageEnabledMember(clusterMemberGroup);

                    } else if (command.startsWith(MERGE_EXTEND_PROXY_COMMAND)) {
                        mergeExtendProxyMember(clusterMemberGroup, command);

                    } else if (command.equals(HELP_COMMAND)) {
                        outputHelp();

                    } else if (command.equals(BYE_COMMAND) || command.equals(QUIT_COMMAND)) {
                        exit = true;

                    } else if (command.startsWith(COMMENT_COMMAND)) {
                        out.println(command);

                    } else if (command.equals(COHQL_COMMAND)) {
                        cohQl();

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

    private void cohQl()
            throws Exception {

        QueryPlus.main(new String[]{});
    }

    private void outputHelp() {
        out.println(format("%s n, m - stops the specified cluster member(s)", STOP_MEMBER_COMMAND));
        out.println(format("%s n, m - shuts down the specified cluster member(s)", SHUTDOWN_MEMBER_COMMAND));
        out.println(format("%s - stops all cluster member(s)", STOP_ALL_COMMAND));
        out.println(format("%s - shuts down all cluster member(s)", SHUTDOWN_ALL_COMMAND));
        out.println(format("%s - exits this application - same as %s", BYE_COMMAND, QUIT_COMMAND));
        out.println(format("%s - quits this application - same as %s", QUIT_COMMAND, BYE_COMMAND));
        out.println(format("%s - displays member Ids known to this application", GET_STARTED_MEMBER_IDS_COMMAND));
        out.println(format("%s n - sleeps for the specified time in seconds", SLEEP_COMMAND));
        out.println(format("%s - merges storage enabled member into this application", MERGE_STORAGE_ENABLED_COMMAND));
        out.println(format("%s n - merges Extend proxy member with specified port into this application",
                MERGE_EXTEND_PROXY_COMMAND));
        out.println(format("%s - displays this help", HELP_COMMAND));
        out.println(format("%s - a comment line, useful when scripting and wanting to comment scripts",
                COMMENT_COMMAND));
        out.println(format("%s - launches CohQL console", COHQL_COMMAND));
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

    private void shutdownAll(final ClusterMemberGroup memberGroup) {
        memberGroup.shutdownAll();
    }

    private void stopAll(final ClusterMemberGroup memberGroup) {
        memberGroup.stopAll();
    }

    private void sleep(final String command)
            throws InterruptedException {

        final int sleepTime = parseInteger(SLEEP_COMMAND, command);

        out.println(format("About to sleep for %d seconds", sleepTime));
        TimeUnit.SECONDS.sleep(sleepTime);
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
        final String[] stringNumbers = commaDelimitedNumbers.split(",");

        final int[] numbers = new int[stringNumbers.length];

        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = Integer.parseInt(stringNumbers[i].trim());
        }

        return numbers;
    }
}
