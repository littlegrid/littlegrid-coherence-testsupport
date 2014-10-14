package org.littlegrid.app;

import org.littlegrid.impl.Info;
import org.littlegrid.management.ManagementService;
import org.littlegrid.management.ManagementUtils;

import java.io.InputStream;
import java.io.PrintStream;
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
    private static final int MILLISECONDS_IN_SECOND = 1000;

    private static final String CREATE_SNAPSHOT = "create snapshot ";
    private static final String DROP_SNAPSHOT = "drop snapshot ";
    private static final String SHOW_SNAPSHOTS = "show snapshots";

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
            final String command = untrimmedCommand.trim();
            String outputResponse;

            try {
                if (command.equals(BYE_COMMAND)
                        || command.equals(QUIT_COMMAND)
                        || command.equals(EXIT_COMMAND)) {

                    outputResponse = command;
                    response.requestExit();
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(SELECT_COMMAND)) {
                    outputResponse = select(command);
                    response.incrementValidCommandsExecuted();

                } else if (command.startsWith(COMMENT_COMMAND)) {
                    outputResponse = command;
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(HELP_COMMAND)) {
                    outputResponse = displayHelp();
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(CREATE_SNAPSHOT)) {
                    outputResponse = createSnapshot(command);
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(DROP_SNAPSHOT)) {
                    outputResponse = dropSnapshot(command);
                    response.incrementCommentCommandsExecuted();

                } else if (command.startsWith(SHOW_SNAPSHOTS)) {
                    outputResponse = showSnapshots();
                    response.incrementCommentCommandsExecuted();

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

    private String createSnapshot(final String command) {
        final String snapshotNameAndQuery = command.replace(CREATE_SNAPSHOT, "").trim();
        final int firstSpaceIndex = snapshotNameAndQuery.indexOf(" ");
        final String snapshotName = snapshotNameAndQuery.substring(0, firstSpaceIndex);
        final String snapshotQuery = snapshotNameAndQuery.substring(firstSpaceIndex).trim();

        return new Integer(managementService.createManagementInformationSnapshot(snapshotName,
                snapshotQuery)).toString();
    }

    private String dropSnapshot(String command) {
        final String snapshotName = command.replace(DROP_SNAPSHOT, "").trim();

        return new Boolean(managementService.dropManagementInformationSnapshot(snapshotName)).toString();
    }

    private String showSnapshots() {
        return managementService.findSnapshots().toString();
    }

    private String displayHelp() {
        return "just select at the moment, e.g. "
                + "select get('nodeId'), get('RequestMaxDuration') "
                + "from 'Coherence:type=Service,name=DistributedCache,nodeId=*'";
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
