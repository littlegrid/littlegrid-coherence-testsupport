package org.littlegrid.common;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Logger;

/**
 * Wrapper around a logger to provide consistent messages containing build information,
 * in the event of more severe log levels, additional information is also output.
 */
public final class LoggerPlaceHolder {
    private final Logger logger;

    /**
     * Constructor.
     *
     * @param name Logger name.
     */
    public LoggerPlaceHolder(final String name) {
        this.logger = Logger.getLogger(name);
    }

    /**
     * Output debug.
     *
     * @param message Message.
     */
    public void fine(final Object message) {
        logger.fine(message.toString());
    }

    /**
     * Output info.
     *
     * @param message Message.
     */
    public void info(final Object message) {
        logger.info(message.toString());
    }

    /**
     * Output warn.
     *
     * @param message Message.
     */
    public void warning(final Object message) {
        logger.warning(message.toString());
    }

    /**
     * Output error.
     *
     * @param message Message.
     */
    public void severe(final Object message) {
        StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        logger.severe(sb.toString());
    }

    private StringBuilder getMessageAndAdditionalInformationToLog(final Object message) {
        StringBuilder sb = new StringBuilder(message.toString());
        sb.append(getAdditionalInformationToLog());

        return sb;
    }

    private String getAdditionalInformationToLog() {
        final int oneMB = 1024 * 1024;

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

        sb.append(".  Free memory: ");
        sb.append(Runtime.getRuntime().freeMemory() / oneMB);
        sb.append("MB");

        return sb.toString();
    }
}
