package org.littlegrid.app;

import org.littlegrid.impl.Info;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.ManagementUtils;
import org.littlegrid.management.TabularResultSet;
import org.littlegrid.management.impl.DefaultTabularResultSet;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.logging.Logger;

import static java.lang.String.format;

/**
 * Management DSL shell
 *
 * @since 2.16
 */
class ManagementDslShell implements Shell {
    private static final Logger LOGGER = Logger.getLogger(ManagementDslShell.class.getName());
    private static final int WAIT_MILLISECONDS_AFTER_STOP_COMMAND = 750;

    private static final String COMMANDS_ARGUMENT = "commands=";
    private static final String COMMAND_DELIMITER = ";";
    private static final String NUMBER_DELIMITER = " ";
    private static final String TIME_DELIMITER = ":";

    private static final String COMMAND_PROMPT = "lgm> ";

    private static final String BYE_COMMAND = "bye";
    private static final String QUIT_COMMAND = "quit";
    private static final String EXIT_COMMAND = "exit";
    private static final String SELECT_COMMAND = "select";
    private static final String SLEEP_COMMAND = "sleep";
    private static final String SLEEP_UNTIL_COMMAND = "sleep until";
    private static final String DATE_COMMAND = "date";
    private static final String HELP_COMMAND = "help";
    private static final String COMMENT_COMMAND = "#";
    private static final String HISTORY_COMMAND = "!";
    private static final String RE_RUN_COMMAND = "!";
    private static final String ALIAS_COMMAND = "@";
    private static final String DESC_COMMAND = "desc";

    private static final int MILLISECONDS_IN_SECOND = 1000;

    private static final String CREATE_SNAPSHOT_COMMAND = "create snapshot ";
    private static final String DROP_SNAPSHOT_COMMAND = "drop snapshot ";
    private static final String SHOW_SNAPSHOTS_COMMAND = "show snapshots";

    /**
     * Text to indicate an unknown command was used.
     */
    static final String COMMAND_UNKNOWN = "COMMAND_UNKNOWN:";

    /**
     * Text to indicate that a command caused an exception.
     */
    static final String COMMAND_EXCEPTION = "COMMAND_EXCEPTION:";

    private final Input in;
    private final Output out;
    private final ManagementService managementService;

    //TODO: think this through more
    private final Map<String, String> previousValidCommands = new LinkedHashMap<String, String>();

    public ManagementDslShell(final InputStream in,
                              final PrintStream out) {

        this(in, out, ManagementUtils.newManagementBuilder()
                .buildAndConnect());
    }

    public ManagementDslShell(final InputStream in,
                              final PrintStream out,
                              final ManagementService managementService) {

        this(new DefaultInput(in), new DefaultOutput(out), managementService);
    }

    /**
     * Constructor.
     *
     * @param in  Input.
     * @param out Output.
     * @since 2.16
     */
    public ManagementDslShell(final Input in,
                              final Output out,
                              final ManagementService managementService) {

        this.in = in;
        this.out = out;
        this.managementService = managementService;
    }

    /**
     * Starts the shell to process commands.
     *
     * @param args Commands passed for execution.
     * @return response to commands.
     */
    @Override
    public Response start(final String[] args) {
        final Response totalResponse = new DefaultResponse();
        final String commands = parseCommandsString(args);
        final Response commandStringResponse = processCommandsString(commands);
        totalResponse.merge(commandStringResponse);

        if (!commandStringResponse.isExitRequested()) {
            out.printlnInfo(format("littlegrid (%s) ManagementDSL shell ready - for list of commands type: help",
                    Info.getVersionNumber()));

            out.printlnInfo("This console is ALPHA CODE and is being used to try out ideas that may go into liittlegrid");
            out.printlnInfo("This console is NOT FOR PRODUCTION USE or any use where you could either get told off or sacked!");

            final Response commandStreamResponse = processCommandsStream();
            totalResponse.merge(commandStreamResponse);
        }

        LOGGER.info(totalResponse.toString());

        return totalResponse;
    }

    private Response processCommandsStream() {
        final Response totalResponse = new DefaultResponse();

        do {
            out.printlnInfo("");
            out.printInfo(COMMAND_PROMPT);

            try {
                final String stringEntered = in.readln();
                final Response response = processCommandsString(stringEntered);

                totalResponse.merge(response);
            } catch (Exception e) {
                out.printlnResponse("No exit request has been made, however no more commands to process - exiting");

                totalResponse.requestExit();
            }
        } while (!totalResponse.isExitRequested());

        return totalResponse;
    }

    private Response processCommandsString(final String stringEntered) {

        final Response response = new DefaultResponse();
        final String[] commands = stringEntered.split(COMMAND_DELIMITER);

        for (String untrimmedCommand : commands) {
            final String candidateCommand = untrimmedCommand.trim();
            final String command;

            if (candidateCommand.startsWith(RE_RUN_COMMAND) && candidateCommand.length() > 1) {
                //TODO: this could be better
                command = previousValidCommands.get(candidateCommand);
            } else {
                command = candidateCommand;
            }

            String outputResponse;

            try {
                if (command.equals(BYE_COMMAND)
                        || command.equals(QUIT_COMMAND)
                        || command.equals(EXIT_COMMAND)) {

                    outputResponse = command;
                    response.requestExit();
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(ALIAS_COMMAND)) {
                    outputResponse = alias(command);
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.startsWith(SELECT_COMMAND)) {
                    outputResponse = select(command);
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.startsWith(COMMENT_COMMAND)) {
                    outputResponse = command;
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(HELP_COMMAND)) {
                    outputHelp();
                    outputResponse = "";
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(CREATE_SNAPSHOT_COMMAND)) {
                    outputResponse = createSnapshot(command);
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.startsWith(DROP_SNAPSHOT_COMMAND)) {
                    outputResponse = dropSnapshot(command);
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.startsWith(SHOW_SNAPSHOTS_COMMAND)) {
                    outputResponse = showSnapshots();
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.startsWith(DESC_COMMAND)) {
                    outputResponse = desc(command);
                    response.incrementValidCommandsExecuted();
                    addToPreviousCommands(command);

                } else if (command.equals(HISTORY_COMMAND)) {
                    outputResponse = showPreviousValidCommands();
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
                out.printlnResponse(outputResponse);
            }
        }

        return response;
    }

    private String alias(String command) {
        return select(command);
    }

    private String desc(final String command) {
        final String snapshotName = parseSnapshotName(DESC_COMMAND, command);

        return managementService.describeSnapshot(snapshotName).toString();
    }

    private void addToPreviousCommands(final String command) {
        final int nextNumber = previousValidCommands.size() + 1;
        previousValidCommands.put(RE_RUN_COMMAND + nextNumber, command);
    }

    private String showPreviousValidCommands() {
        final TabularResultSet history = new DefaultTabularResultSet();

        for (final Map.Entry<String, String> entry : previousValidCommands.entrySet()) {
            final Map<String, Object> row = new HashMap<String, Object>();
            row.put(entry.getKey(), entry.getValue());

            history.addRow(row);
        }

        return history.toString();
    }

    @Deprecated //TODO: refactor to re-use
    private static int parseInteger(final String command,
                                    final String commandAndNumber) {

        return parseIntegers(command, NUMBER_DELIMITER, commandAndNumber)[0];
    }

    @Deprecated //TODO: refactor to re-use
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

    private String createSnapshot(final String command) {
        //TODO: use parse snapshot name
        final String snapshotNameAndQuery = command.replace(CREATE_SNAPSHOT_COMMAND, "").trim();
        final int firstSpaceIndex = snapshotNameAndQuery.indexOf(" ");
        final String snapshotName = snapshotNameAndQuery.substring(0, firstSpaceIndex);
        final String snapshotQuery = snapshotNameAndQuery.substring(firstSpaceIndex).trim();

        return new Integer(managementService.createManagementInformationSnapshot(snapshotName,
                snapshotQuery)).toString();
    }

    private String parseSnapshotName(final String keyword,
                                     final String command) {

        final String snapshotName = command.replace(keyword, "").trim();

        return snapshotName;
    }

    private String dropSnapshot(String command) {
        //TODO: use parse snapshot name
        final String snapshotName = command.replace(DROP_SNAPSHOT_COMMAND, "").trim();

        return new Boolean(managementService.dropManagementInformationSnapshot(snapshotName)).toString();
    }

    private String showSnapshots() {
        return managementService.findSnapshots().toString();
    }

    private void outputHelp() {

        out.printlnInfo(format("%s - exits application - same as %s and %s", BYE_COMMAND, QUIT_COMMAND, EXIT_COMMAND));
        out.printlnInfo(format("%s - exits application - same as %s and %s", QUIT_COMMAND, BYE_COMMAND, EXIT_COMMAND));
        out.printlnInfo(format("%s - exits application - same as %s and %s", EXIT_COMMAND, QUIT_COMMAND, BYE_COMMAND));

/*
        out.printlnInfo(format("%s duration_X - sleeps for the specified time in milliseconds, e.g. 1000", SLEEP_COMMAND));
        out.printlnInfo(format("%s HH:MI:SS - sleeps until the specified time, e.g. 18:01:02", SLEEP_UNTIL_COMMAND));
*/

        out.printlnInfo(format("%s - displays this help", HELP_COMMAND));
/*
        out.printlnInfo(format("%s - displays the current date and time", DATE_COMMAND));
*/
        out.printlnInfo(format("%s - a comment line, useful when scripting and wanting to comment scripts",
                COMMENT_COMMAND));

    }

    private String select(final String command) {
        return managementService.findManagementInformation(command).toString();
    }

    private String parseCommandsString(final String[] args) {
        for (final String argument : args) {
            if (argument.startsWith(COMMANDS_ARGUMENT)) {
                return argument.replaceAll(COMMANDS_ARGUMENT, "");
            }
        }

        return "";
    }


    /**
     * Response object.
     *
     * @since 2.15
     */
    static class DefaultResponse implements Response {
        private int validCommandsExecuted;
        private int invalidCommandsExecuted;
        private int unknownCommandsExecuted;
        private int commentCommandsExecuted;
        private boolean exitRequested;

        /**
         * {@code}
         */
        @Override
        public void incrementValidCommandsExecuted() {
            validCommandsExecuted++;
        }

        /**
         * {@code}
         */
        @Override
        public void incrementInvalidCommandsExecuted() {
            invalidCommandsExecuted++;
        }

        /**
         * {@code}
         */
        @Override
        public void incrementUnknownCommandsExecuted() {
            unknownCommandsExecuted++;
        }

        /**
         * {@code}
         */
        @Override
        public void incrementCommentCommandsExecuted() {
            commentCommandsExecuted++;
        }

        /**
         * {@code}
         */
        @Override
        public void requestExit() {
            exitRequested = true;
        }

        /**
         * {@code}
         */
        @Override
        public int getValidCommandsExecuted() {
            return validCommandsExecuted;
        }

        /**
         * {@code}
         */
        @Override
        public int getInvalidCommandsExecuted() {
            return invalidCommandsExecuted;
        }

        /**
         * {@code}
         */
        @Override
        public int getUnknownCommandsExecuted() {
            return unknownCommandsExecuted;
        }

        /**
         * {@code}
         */
        @Override
        public int getCommentCommandsExecuted() {
            return commentCommandsExecuted;
        }

        /**
         * {@code}
         */
        @Override
        public boolean isExitRequested() {
            return exitRequested;
        }

        /**
         * {@code}
         */
        @Override
        public void merge(final Response otherResponse) {
            validCommandsExecuted += otherResponse.getValidCommandsExecuted();
            invalidCommandsExecuted += otherResponse.getInvalidCommandsExecuted();
            unknownCommandsExecuted += otherResponse.getUnknownCommandsExecuted();
            commentCommandsExecuted += otherResponse.getCommentCommandsExecuted();

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

    static class DefaultInput implements Input {
        private final Scanner scanner;

        /**
         * Constructor.
         *
         * @param inputStream Input stream.
         */
        public DefaultInput(final InputStream inputStream) {
            scanner = new Scanner(inputStream);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String readln() {
            return scanner.nextLine();
        }
    }

    static class DefaultOutput implements Output {
        private final PrintStream printStream;

        public DefaultOutput(final PrintStream printStream) {
            this.printStream = printStream;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printResponse(final String message) {
            printStream.print(message);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printlnResponse(final String message) {
            printStream.println(message);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printInfo(final String message) {
            printStream.print(message);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void printlnInfo(final String message) {
            printStream.println(message);
        }
    }
}
