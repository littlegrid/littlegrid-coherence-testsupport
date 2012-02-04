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

package org.littlegrid.utils;

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
    public void debug(final Object message) {
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
    public void warn(final Object message) {
        logger.warning(message.toString());
    }

    /**
     * Output error.
     *
     * @param message Message.
     */
    public void error(final Object message) {
        final StringBuilder sb = getMessageAndAdditionalInformationToLog(message);

        logger.severe(sb.toString());
    }

    private StringBuilder getMessageAndAdditionalInformationToLog(final Object message) {
        final StringBuilder sb = new StringBuilder(message.toString());

        sb.append(getAdditionalInformationToLog());

        return sb;
    }

    private String getAdditionalInformationToLog() {
        final int oneMB = 1024 * 1024;

        final StringBuilder sb = new StringBuilder(".  Additional info: ");

        try {
            final InetAddress inetAddress = InetAddress.getLocalHost();

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
