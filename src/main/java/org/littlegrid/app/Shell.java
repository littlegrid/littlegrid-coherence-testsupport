package org.littlegrid.app;

/**
 * Shell interface.
 *
 * @since 2.16
 */
public interface Shell {
    /**
     * Starts a shell.
     *
     * @param args Arguments.
     * @return response overview.
     */
    Response start(String[] args);

    /**
     * Shell input.
     */
    interface Input {
        String readln();
    }

    /**
     * Shell output.
     */
    interface Output {
        /**
         * Print a response.
         *
         * @param message Message.
         */
        void printResponse(String message);

        /**
         * Print a response.
         *
         * @param message Message.
         */
        void printlnResponse(String message);

        /**
         * Prints information.
         *
         * @param message Message.
         */
        void printInfo(String message);

        /**
         * Prints information.
         *
         * @param message Message.
         */
        void printlnInfo(String message);
    }

    /**
     * Response.
     */
    interface Response {
        /**
         * Increments the number of valid commands executed.
         */
        void incrementValidCommandsExecuted();

        /**
         * Increments the number of invalid commands executed.
         */
        void incrementInvalidCommandsExecuted();

        /**
         * Increments the number of unknown commands executed.
         */
        void incrementUnknownCommandsExecuted();

        /**
         * Increments the number of unknown commands executed.
         */
        void incrementCommentCommandsExecuted();

        /**
         * Indicates that exit is being requested.
         */
        void requestExit();

        /**
         * Number of valid commands executed.
         *
         * @return number of valid commands executed.
         */
        int getValidCommandsExecuted();

        /**
         * Number of invalid commands executed, these are commands where the instruction
         * has been recognised, but the syntax is incorrect, for instance a string is
         * provided instead of an expected number argument.
         *
         * @return number of invalid commands executed.
         */
        int getInvalidCommandsExecuted();

        /**
         * Number of unknown commands executed, these are commands where the instruction
         * has not been recognised and is not one of the supported commands.
         *
         * @return number of unknown commands executed.
         */
        int getUnknownCommandsExecuted();

        /**
         * Number of comments executed.
         *
         * @return number of comments executed.
         */
        int getCommentCommandsExecuted();

        /**
         * Has exit been requested, this is where one of the instructions to quit
         * has been specified and the shell should exit.
         *
         * @return true if exit requested.
         */
        boolean isExitRequested();

        /**
         * Merges another response with this response to combine the totals commands
         * executed.
         *
         * @param otherResponse other response.
         */
        void merge(final Response otherResponse);
    }
}
