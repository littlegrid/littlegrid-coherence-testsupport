package org.testsupport.coherence.impl;


import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Logger;

/**
 * Wrapper around a logger to provide consistent messages containing build information, in the event
 * of more severe log levels, additional information is also output.
 */
public class LoggerWrapper {
    private final String messageStart;
    private final Logger logger;

    /**
     * Constructor.
     *
     * @param buildInfo Build information.
     * @param logger    Logger being wrapped.
     */
    public LoggerWrapper(String buildInfo,
                         Logger logger) {

        this.messageStart = buildInfo + " ";
        this.logger = logger;
    }

    /**
     * Checks if debug level is enabled.
     *
     * @return status.
     */
    public boolean isDebugEnabled() {
        throw new UnsupportedOperationException();
//        return logger.isDebugEnabled();
    }

    /**
     * Output debug.
     *
     * @param message Message.
     */
    public void fine(Object message) {
        StringBuilder sb = new StringBuilder(messageStart);
        sb.append(message);

        logger.fine(sb.toString());
    }

    /**
     * Output info.
     *
     * @param message Message.
     */
    public void info(Object message) {
        StringBuilder sb = new StringBuilder(messageStart);
        sb.append(message);

        logger.info(sb.toString());
    }

    /**
     * Output warn.
     *
     * @param message Message.
     */
    public void warning(Object message) {
        StringBuilder sb = new StringBuilder(messageStart);
        sb.append(message);

        logger.warning(sb.toString());
    }

    /**
     * Output error.
     *
     * @param message Message.
     */
    public void severe(Object message) {
        StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        logger.severe(sb.toString());
    }

    /**
     * Output error.
     *
     * @param message   Message.
     * @param throwable Throwable.
     */
    public void error(Object message, Throwable throwable) {
        StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        throw new UnsupportedOperationException();
//        logger.severe(sb.toString(), throwable);
    }

    /**
     * Output fatal.
     *
     * @param message Message.
     */
    public void fatal(Object message) {
        StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        throw new UnsupportedOperationException();
//        logger.fatal(sb.toString());
    }

    /**
     * Output fatal.
     *
     * @param message   Message.
     * @param throwable Throwable.
     */
    public void fatal(Object message, Throwable throwable) {
        StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        throw new UnsupportedOperationException();
//        logger.fatal(sb.toString(), throwable);
    }

    private StringBuilder getMessageAndAdditionalInformationToLog(Object message) {
        StringBuilder sb = new StringBuilder(messageStart);
        sb.append(message);
        sb.append(getAdditionalInformationToLog());
        return sb;
    }

    private String getAdditionalInformationToLog() {
        StringBuilder sb = new StringBuilder(".  Additional info: ");

        try {
            InetAddress inetAddress = InetAddress.getLocalHost();

            sb.append(inetAddress.getHostName());
            sb.append(" (");
            sb.append(inetAddress.getHostAddress());
            sb.append(")");
        } catch (UnknownHostException e) {
            sb.append("Unable to determine hostname/IP address");
        }

        return sb.toString();
    }
}
