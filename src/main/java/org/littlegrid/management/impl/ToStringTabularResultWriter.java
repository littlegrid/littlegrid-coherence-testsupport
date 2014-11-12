package org.littlegrid.management.impl;

import org.littlegrid.management.TabularResult;
import org.littlegrid.management.TabularResultWriter;

import java.io.IOException;
import java.io.Writer;

/**
 * Simple toString tabular result writer.
 *
 * @since 2.16
 */
public class ToStringTabularResultWriter implements TabularResultWriter {
    private final Writer writer;

    /**
     * Constructor.
     *
     * @param writer Writer.
     */
    public ToStringTabularResultWriter(final Writer writer) {
        this.writer = writer;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int apply(final TabularResult result) {
        try {
            writer.write(result.toString());

            return result.getRowCount();
        } catch (IOException e) {
            throw new UnsupportedOperationException();
            //TODO:
        }
    }
}
