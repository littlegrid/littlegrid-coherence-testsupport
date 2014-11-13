package org.littlegrid.management.impl;

import org.littlegrid.management.TabularResult;
import org.littlegrid.management.TabularResultWriter;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Delimited string tabular result writer.
 *
 * @since 2.16
 */
public class DelimitedStringTabularResultWriter implements TabularResultWriter {
    private static final String NEW_LINE = "\n";
    private static final String DELIMITER = "|";

    private final Writer writer;
    private final boolean outputHeading;

    /**
     * Constructor.
     *
     * @param writer Writer.
     */
    public DelimitedStringTabularResultWriter(final Writer writer) {
        this(writer, true);
    }

    /**
     * Constructor.
     *
     * @param writer        Writer.
     * @param outputHeading Determines if heading should be output.
     */
    public DelimitedStringTabularResultWriter(final Writer writer,
                                              final boolean outputHeading) {

        this.writer = writer;
        this.outputHeading = outputHeading;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int apply(final TabularResult result) {
        try {
            if (outputHeading) {
                applyHeading(result);
            }

            int rowsWritten = 0;

            for (final Map<String, Object> row : result.getRows()) {
                final List<Object> values = new ArrayList<Object>(row.values());
                final int totalValues = values.size();

                for (int columnNumber = 0; columnNumber < totalValues; columnNumber++) {
                    final Object value = values.get(columnNumber);

                    if (value != null) {
                        writer.write(value.toString());
                    }

                    if (columnNumber < (totalValues - 1)) {
                        writer.write(DELIMITER);
                    }
                }

                writer.write(NEW_LINE);
                rowsWritten++;
            }

            return rowsWritten;
        } catch (IOException e) {
            throw new UnsupportedOperationException();
            //TODO:
        }
    }

    private void applyHeading(final TabularResult result)
            throws IOException {

        final List<String> columnNames = new ArrayList<String>(result.getColumnNames());

        for (int columnNumber = 0; columnNumber < result.getColumnCount(); columnNumber++) {
            final String columnName = columnNames.get(columnNumber);

            writer.write(columnName);

            if (columnNumber < (columnNames.size() - 1)) {
                writer.write(DELIMITER);
            }
        }

        writer.write(NEW_LINE);
    }
}
