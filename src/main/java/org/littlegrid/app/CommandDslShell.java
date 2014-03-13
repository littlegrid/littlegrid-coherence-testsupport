/*
 * Copyright (c) 2010-2014 Jonathan Hall.
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
import org.littlegrid.impl.Info;

import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import static java.lang.String.format;
import static org.littlegrid.ClusterMemberGroup.Builder;

/**
 * Command DSL shell - purposely reduced scope for the methods to keep visible 'surface-area'
 * small for now and not expose internals that may change in future versions.
 *
 * @since 2.15
 */
class CommandDslShell {
    private static final Logger LOGGER = Logger.getLogger(CommandDslShell.class.getName());
    private static final int WAIT_MILLISECONDS_AFTER_STOP_COMMAND = 750;

    private static final String COMMANDS_ARGUMENT = "commands=";
    private static final String COMMAND_DELIMITER = ";";
    private static final String NUMBER_DELIMITER = " ";
    private static final String TIME_DELIMITER = ":";

    private static final String COMMAND_PROMPT = "lg> ";

    private static final String STOP_MEMBER_COMMAND = "stop member";
    private static final String BYE_COMMAND = "bye";
    private static final String QUIT_COMMAND = "quit";
    private static final String EXIT_COMMAND = "exit";
    private static final String GET_STARTED_MEMBER_IDS_COMMAND = "members";
    private static final String SHUTDOWN_ALL_COMMAND = "shutdown all";
    private static final String STOP_ALL_COMMAND = "stop all";
    private static final String SLEEP_COMMAND = "sleep";
    private static final String SLEEP_UNTIL_COMMAND = "sleep until";
    private static final String SHUTDOWN_MEMBER_COMMAND = "shutdown member";
    private static final String START_STORAGE_ENABLED_COMMAND = "start storage enabled";
    private static final String START_MULTIPLE_STORAGE_ENABLED_COMMAND = "start storage enabled * ";
    private static final String START_EXTEND_PROXY_COMMAND = "start extend proxy";
    private static final String START_JMX_MONITOR_COMMAND = "start jmx monitor";
    private static final String DATE_COMMAND = "date";
    private static final String HELP_COMMAND = "help";
    private static final String COMMENT_COMMAND = "#";
    private static final String CONSOLE_COMMAND = "console";
    private static final String COHQL_COMMAND = "cohql";
    private static final String SITE_COMMAND = "site =";
    private static final String RACK_COMMAND = "rack =";
    private static final String MACHINE_COMMAND = "machine =";
    private static final int MILLISECONDS_IN_SECOND = 1000;
    private static final String QUERY_PLUS_CLASS_NAME = "com.tangosol.coherence.dslquery.QueryPlus";
    private static final String COHERENCE_CONSOLE_CLASS_NAME =
            "com.tangosol.coherence.component.application.console.Coherence";

    /**
     * Text to indicate an unknown command was used.
     */
    public static final String COMMAND_UNKNOWN = "COMMAND_UNKNOWN:";

    /**
     * Text to indicate that a command caused an exception.
     */
    public static final String COMMAND_EXCEPTION = "COMMAND_EXCEPTION:";

    private final InputStream in;
    private final PrintStream out;
    private final ClusterMemberGroup memberGroup;
    private final List<String> commandsToIgnore;
    private final boolean outputPrompts;

    private String site;
    private String rack;
    private String machine;

    /**
     * Constructor.
     *
     * @param in  Input stream.
     * @param out Output stream.
     */
    public CommandDslShell(final InputStream in,
                           final PrintStream out) {

        this(in, out, ClusterMemberGroupUtils.newBuilder().buildAndConfigure(), new ArrayList<String>(), true);
    }

    /**
     * Constructor.
     *
     * @param in               Input stream.
     * @param out              Output stream.
     * @param memberGroup      Cluster member group.
     * @param commandsToIgnore Command that should be ignored based upon the context of the shell usage.
     * @param outputPrompts    Indicates if user prompts should be output.
     * @since 2.16
     */
    public CommandDslShell(final InputStream in,
                           final PrintStream out,
                           final ClusterMemberGroup memberGroup,
                           final List<String> commandsToIgnore,
                           final boolean outputPrompts) {

        this.in = in;
        this.out = out;
        this.memberGroup = memberGroup;
        this.commandsToIgnore = commandsToIgnore;
        this.outputPrompts = outputPrompts;
    }

    /**
     * Starts the shell to process commands.
     *
     * @param args Commands passed for execution.
     * @return response to commands.
     */
    public Response start(final String[] args) {
        final Response totalResponse = new Response();
        final String commands = parseCommandsString(args);
        final Response commandStringResponse = processCommandsString(memberGroup, commands);
        totalResponse.merge(commandStringResponse);

        if (!commandStringResponse.isExitRequested()) {
            if (outputPrompts) {
                out.println(format("littlegrid (%s) DSL shell ready - for list of commands type: help",
                        Info.getVersionNumber()));
            }

            final Scanner scanner = new Scanner(in);

            final Response commandStreamResponse = processCommandsStream(memberGroup, scanner);
            totalResponse.merge(commandStreamResponse);
        }

        LOGGER.info(totalResponse.toString());

        ClusterMemberGroupUtils.shutdownCacheFactoryThenClusterMemberGroups(memberGroup);

        return totalResponse;
    }

    private String parseCommandsString(final String[] args) {
        for (final String argument : args) {
            if (argument.startsWith(COMMANDS_ARGUMENT)) {
                return argument.replaceAll(COMMANDS_ARGUMENT, "");
            }
        }

        return "";
    }

    private Response processCommandsStream(final ClusterMemberGroup memberGroup,
                                           final Scanner scanner) {

        final Response totalResponse = new Response();

        do {
            if (outputPrompts) {
                out.println();
                out.print(COMMAND_PROMPT);
            }

            try {
                final String stringEntered = scanner.nextLine();
                final Response response = processCommandsString(memberGroup, stringEntered);

                totalResponse.merge(response);
            } catch (Exception e) {
                out.println("No exit request made, but no more commands to process - exiting");

                totalResponse.requestExit();
            }
        } while (!totalResponse.isExitRequested());

        return totalResponse;
    }

    private Response processCommandsString(final ClusterMemberGroup memberGroup,
                                           final String stringEntered) {

        final Response response = new Response();
        final String[] commands = stringEntered.split(COMMAND_DELIMITER);

        for (String untrimmedCommand : commands) {
            final String command = untrimmedCommand.trim();
            String outputResponse;

            try {
                if (command.startsWith(STOP_MEMBER_COMMAND)) {
                    outputResponse = stopMember(memberGroup, command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(SHUTDOWN_MEMBER_COMMAND)) {
                    outputResponse = shutdownMember(memberGroup, command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(SLEEP_UNTIL_COMMAND)) {
                    outputResponse = sleepUntil(command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(SLEEP_COMMAND)) {
                    outputResponse = sleep(command);
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(STOP_ALL_COMMAND)) {
                    outputResponse = stopAll(memberGroup);
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(SHUTDOWN_ALL_COMMAND)) {
                    outputResponse = shutdownAll(memberGroup);
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(GET_STARTED_MEMBER_IDS_COMMAND)) {
                    outputResponse = getStartedMemberIds(memberGroup);
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(START_STORAGE_ENABLED_COMMAND)) {
                    outputResponse = startStorageEnabledMember(memberGroup);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(START_MULTIPLE_STORAGE_ENABLED_COMMAND)) {
                    outputResponse = startMultipleStorageEnabledMembers(memberGroup, command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(START_EXTEND_PROXY_COMMAND)) {
                    outputResponse = startExtendProxyMember(memberGroup, command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(START_JMX_MONITOR_COMMAND)) {
                    outputResponse = startJmxMonitorMember(memberGroup);
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(HELP_COMMAND)) {
                    outputHelp();
                    outputResponse = "";
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(BYE_COMMAND)
                        || command.equals(QUIT_COMMAND)
                        || command.equals(EXIT_COMMAND)) {

                    outputResponse = command;
                    response.requestExit();
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(COMMENT_COMMAND)) {
                    outputResponse = command;
                    response.incrementCommentCommandsExecuted();

                } else if (command.equals(CONSOLE_COMMAND)) {
                    console();
                    outputResponse = "";
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(COHQL_COMMAND)) {
                    cohQl();
                    outputResponse = "";
                    response.incrementValidCommandsExecuted();

                } else if (command.equals(DATE_COMMAND)) {
                    outputResponse = getDate();
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(SITE_COMMAND)) {
                    setSite(command);
                    outputResponse = command;
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(RACK_COMMAND)) {
                    setRack(command);
                    outputResponse = command;
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(MACHINE_COMMAND)) {
                    setMachine(command);
                    outputResponse = command;
                    response.incrementValidCommandsExecuted();

                } else if (command.equals("")) {
                    outputResponse = "";

                } else {
                    outputResponse = format(COMMAND_UNKNOWN + " '%s'", command);
                    response.incrementUnknownCommandsExecuted();
                }
            } catch (Exception e) {
                outputResponse = format(COMMAND_EXCEPTION + " '%s' due to: %s", command, e);
                response.incrementInvalidCommandsExecuted();
            }

            if (!outputResponse.isEmpty()) {
                out.println(outputResponse);
            }
        }

        return response;
    }

    private void setSite(final String command) {
        this.site = command.replaceAll(SITE_COMMAND, "").trim();
    }

    private void setRack(final String command) {
        this.rack = command.replaceAll(RACK_COMMAND, "").trim();
    }

    private void setMachine(final String command) {
        this.machine = command.replaceAll(MACHINE_COMMAND, "").trim();
    }

    private String sleepUntil(final String command)
            throws InterruptedException {

        final int[] timeParts = parseTime(SLEEP_UNTIL_COMMAND, command);

        final Date now = new Date();
        final GregorianCalendar dateToWaitUntil = new GregorianCalendar();

        dateToWaitUntil.setTime(now);
        dateToWaitUntil.set(Calendar.HOUR_OF_DAY, timeParts[0]);
        dateToWaitUntil.set(Calendar.MINUTE, timeParts[1]);
        dateToWaitUntil.set(Calendar.SECOND, timeParts[2]);

        final long sleepTime = (dateToWaitUntil.getTimeInMillis() - now.getTime()) / MILLISECONDS_IN_SECOND;

        out.print(format("Current time %s, now about to sleep for %d seconds - which is: %s",
                now, sleepTime, dateToWaitUntil.getTime()));

        TimeUnit.SECONDS.sleep(sleepTime);

        return format("Finished sleeping, time now: %s", new Date());
    }

    private String getDate() {
        return new Date().toString();
    }

    @SuppressWarnings("unchecked")
    private void console()
            throws Exception {

        CacheFactory.main(new String[]{});

        final Class coherenceConsole = Class.forName(COHERENCE_CONSOLE_CLASS_NAME,
                true, Thread.currentThread().getContextClassLoader());

        final Object component = ClassHelper.invokeStatic(coherenceConsole, "get_Instance", null);
        final Method stopMethod = coherenceConsole.getDeclaredMethod("setStop", boolean.class);

        stopMethod.setAccessible(true);
        stopMethod.invoke(component, false);
    }

    private void cohQl()
            throws Exception {

        final Class cohqlConsole =
                this.getClass().getClassLoader().loadClass(QUERY_PLUS_CLASS_NAME);

        ClassHelper.invokeStatic(cohqlConsole, "main", new Object[]{new String[]{}});
    }

    private void outputHelp() {
        out.println(format("%s - starts a storage enabled member in this process", START_STORAGE_ENABLED_COMMAND));
        out.println(format("%sn - starts the specified number of storage enabled members in this process",
                START_MULTIPLE_STORAGE_ENABLED_COMMAND));

        out.println(format("%s n - starts an Extend proxy member with specified port in this process",
                START_EXTEND_PROXY_COMMAND));

        out.println(format("%s - starts a JMX monitor member in this process", START_JMX_MONITOR_COMMAND));

        out.println(format("%s - displays member Ids known to this process (note: these are only for this process)",
                GET_STARTED_MEMBER_IDS_COMMAND));

        out.println(format("%s member_id_1 member_id_X - stops the specified cluster member(s) using their member id",
                STOP_MEMBER_COMMAND));

        out.println(format("%s member_id_1 member_id_X - shuts down the specified cluster member(s) using their id",
                SHUTDOWN_MEMBER_COMMAND));

        out.println(format("%s - stops all cluster member(s)", STOP_ALL_COMMAND));
        out.println(format("%s - shuts down all cluster member(s)", SHUTDOWN_ALL_COMMAND));

        out.println(format("%s - exits application - same as %s and %s", BYE_COMMAND, QUIT_COMMAND, EXIT_COMMAND));
        out.println(format("%s - exits application - same as %s and %s", QUIT_COMMAND, BYE_COMMAND, EXIT_COMMAND));
        out.println(format("%s - exits application - same as %s and %s", EXIT_COMMAND, QUIT_COMMAND, BYE_COMMAND));

        out.println(format("%s duration_X - sleeps for the specified time in milliseconds, e.g. 1000", SLEEP_COMMAND));
        out.println(format("%s HH:MI:SS - sleeps until the specified time, e.g. 18:01:02", SLEEP_UNTIL_COMMAND));

        out.println(format("%s - displays this help", HELP_COMMAND));
        out.println(format("%s - displays the current date and time", DATE_COMMAND));
        out.println(format("%s - a comment line, useful when scripting and wanting to comment scripts",
                COMMENT_COMMAND));

        out.println(format("%s siteName - sets site name, this will be used by all future started members",
                SITE_COMMAND));

        out.println(format("%s rackName - sets rack name, this will be used by all future started members",
                RACK_COMMAND));

        out.println(format("%s machineName - sets machine name, this will be used by all future started members",
                MACHINE_COMMAND));

        out.println(format("%s - launches CohQL console", COHQL_COMMAND));
        out.println(format("%s - launches Coherence console (not for Extend clients)", CONSOLE_COMMAND));
    }

    private String startJmxMonitorMember(final ClusterMemberGroup memberGroup) {
        memberGroup.merge(getNewBuilder()
                .setJmxMonitorCount(1)
                .buildAndConfigure());

        return getStartedMemberIds(memberGroup);
    }

    private String startExtendProxyMember(final ClusterMemberGroup memberGroup,
                                          final String command) {

        final int extendPort = parseInteger(START_EXTEND_PROXY_COMMAND, command);

        memberGroup.merge(getNewBuilder()
                .setExtendProxyCount(1)
                .setExtendPort(extendPort)
                .buildAndConfigure());

        return getStartedMemberIds(memberGroup);
    }

    private String startStorageEnabledMember(final ClusterMemberGroup memberGroup) {
        memberGroup.merge(getNewBuilder()
                .setStorageEnabledCount(1)
                .buildAndConfigure());

        return getStartedMemberIds(memberGroup);
    }

    private String startMultipleStorageEnabledMembers(final ClusterMemberGroup memberGroup,
                                                      final String command) {

        final int numberOfMembers = parseInteger(START_MULTIPLE_STORAGE_ENABLED_COMMAND, command);

        memberGroup.merge(getNewBuilder()
                .setStorageEnabledCount(numberOfMembers)
                .buildAndConfigure());

        return getStartedMemberIds(memberGroup);
    }

    private Builder getNewBuilder() {
        return ClusterMemberGroupUtils.newBuilder()
                .setSiteName(site)
                .setRackName(rack)
                .setMachineName(machine)
                .setStorageEnabledCount(0)
                .setExtendProxyCount(0)
                .setStorageEnabledExtendProxyCount(0)
                .setJmxMonitorCount(0)
                .setCustomConfiguredCount(0);
    }

    private String shutdownAll(final ClusterMemberGroup memberGroup) {
        memberGroup.shutdownAll();

        return getStartedMemberIds(memberGroup);
    }

    private String stopAll(final ClusterMemberGroup memberGroup) {
        memberGroup.stopAll();

        return getStartedMemberIds(memberGroup);
    }

    private String sleep(final String command)
            throws InterruptedException {

        final int sleepTime = parseInteger(SLEEP_COMMAND, command);

        out.print(format("About to sleep for %d milliseconds", sleepTime));
        TimeUnit.MILLISECONDS.sleep(sleepTime);

        return "";
    }

    private String shutdownMember(final ClusterMemberGroup memberGroup,
                                  final String command) {

        final int[] memberId = parseIntegers(SHUTDOWN_MEMBER_COMMAND, NUMBER_DELIMITER, command);

        memberGroup.shutdownMember(memberId);

        return getStartedMemberIds(memberGroup);
    }

    private String stopMember(final ClusterMemberGroup memberGroup,
                              final String command)
            throws InterruptedException {

        final int[] memberId = parseIntegers(STOP_MEMBER_COMMAND, NUMBER_DELIMITER, command);

        memberGroup.stopMember(memberId);

        TimeUnit.MILLISECONDS.sleep(WAIT_MILLISECONDS_AFTER_STOP_COMMAND);

        return getStartedMemberIds(memberGroup);
    }

    private String getStartedMemberIds(final ClusterMemberGroup memberGroup) {
        return "Started member ids: " + Arrays.toString(memberGroup.getStartedMemberIds());
    }

    private static int parseInteger(final String command,
                                    final String commandAndNumber) {

        return parseIntegers(command, NUMBER_DELIMITER, commandAndNumber)[0];
    }

    private static int[] parseTime(final String command,
                                   final String commandAndTime) {

        final int[] timeParts = parseIntegers(command, TIME_DELIMITER, commandAndTime);

        if (timeParts.length != 3) {
            throw new IllegalArgumentException("Time cannot be parsed");
        }

        return timeParts;
    }

    private static int[] parseIntegers(final String command,
                                       final String delimiter,
                                       final String commandAndDelimitedNumbers) {

        final String delimitedNumbers = commandAndDelimitedNumbers.replaceAll(command, "").replace("*", "");
        final String[] stringNumbers = delimitedNumbers.trim().split(delimiter);

        final int[] numbers = new int[stringNumbers.length];

        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = Integer.parseInt(stringNumbers[i].trim());
        }

        return numbers;
    }

    /**
     * Response object.
     *
     * @since 2.15
     */
    public static class Response {
        private int validCommandsExecuted;
        private int invalidCommandsExecuted;
        private int unknownCommandsExecuted;
        private int commentCommandsExecuted;
        private boolean exitRequested;

        /**
         * Increments the number of valid commands executed.
         */
        public void incrementValidCommandsExecuted() {
            validCommandsExecuted++;
        }

        /**
         * Increments the number of invalid commands executed.
         */
        public void incrementInvalidCommandsExecuted() {
            invalidCommandsExecuted++;
        }

        /**
         * Increments the number of unknown commands executed.
         */
        public void incrementUnknownCommandsExecuted() {
            unknownCommandsExecuted++;
        }

        /**
         * Increments the number of unknown commands executed.
         */
        public void incrementCommentCommandsExecuted() {
            commentCommandsExecuted++;
        }

        /**
         * Indicates that exit is being requested.
         */
        public void requestExit() {
            exitRequested = true;
        }

        /**
         * Number of valid commands executed.
         *
         * @return number of valid commands executed.
         */
        public int getValidCommandsExecuted() {
            return validCommandsExecuted;
        }

        /**
         * Number of invalid commands executed, these are commands where the instruction
         * has been recognised, but the syntax is incorrect, for instance a string is
         * provided instead of an expected number argument.
         *
         * @return number of invalid commands executed.
         */
        public int getInvalidCommandsExecuted() {
            return invalidCommandsExecuted;
        }

        /**
         * Number of unknown commands executed, these are commands where the instruction
         * has not been recognised and is not one of the supported commands.
         *
         * @return number of unknown commands executed.
         */
        public int getUnknownCommandsExecuted() {
            return unknownCommandsExecuted;
        }

        /**
         * Number of comments executed.
         *
         * @return number of comments executed.
         */
        public int getCommentCommandsExecuted() {
            return commentCommandsExecuted;
        }

        /**
         * Has exit been requested, this is where one of the instructions to quit
         * has been specified and the shell should exit.
         *
         * @return true if exit requested.
         */
        public boolean isExitRequested() {
            return exitRequested;
        }

        /**
         * Merges another response with this response to combine the totals commands
         * executed.
         *
         * @param otherResponse other response.
         */
        public void merge(final Response otherResponse) {
            validCommandsExecuted += otherResponse.validCommandsExecuted;
            invalidCommandsExecuted += otherResponse.invalidCommandsExecuted;
            unknownCommandsExecuted += otherResponse.unknownCommandsExecuted;
            commentCommandsExecuted += otherResponse.commentCommandsExecuted;

            if (otherResponse.isExitRequested()) {
                exitRequested = true;
            }
        }

        /**
         * String representation of this object.
         *
         * @return string.
         */
        public String toString() {
            return format("Commands executed - valid: %d, invalid: %d, unknown: %d, comments: %d, exit: %s",
                    validCommandsExecuted, invalidCommandsExecuted, unknownCommandsExecuted, commentCommandsExecuted,
                    exitRequested);
        }
    }

    /**
     * Default scope to facilitate testing.
     *
     * @return site.
     */
    String getSite() {
        return site;
    }

    /**
     * Default scope to facilitate testing.
     *
     * @return rack.
     */
    String getRack() {
        return rack;
    }

    /**
     * Default scope to facilitate testing.
     *
     * @return machine.
     */
    String getMachine() {
        return machine;
    }
}
