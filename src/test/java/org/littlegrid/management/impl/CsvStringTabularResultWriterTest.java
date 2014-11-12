package org.littlegrid.management.impl;

import org.junit.Test;
import org.littlegrid.management.TabularResult;
import org.littlegrid.management.TabularResultWriter;

import java.io.StringWriter;
import java.io.Writer;
import java.util.LinkedHashMap;
import java.util.Map;

import static java.lang.String.format;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * CSV string tabular result writer tests.
 */
public class CsvStringTabularResultWriterTest {
    private static final String NEW_LINE = "\n";
    private static final String DELIMITER = ",";

    private static final String KNOWN_KEY_PREFIX = "some-key-%d";
    private static final String KNOWN_VALUE_PREFIX = "some-value-%d-%d";

    @Test
    public void applyWhenNoRows() {
        applyAndCheckAsExpected(false, 0, 0);
    }

    @Test
    public void applyWhenOneRowAndOneColumnAndWithoutHeading() {
        applyAndCheckAsExpected(false, 1, 1);
    }

    @Test
    public void applyWhenManyRowsAndOneColumnAndWithoutHeading() {
        applyAndCheckAsExpected(false, 10, 1);
    }

    @Test
    public void applyWhenOneRowAndManyColumnsAndWithoutHeading() {
        applyAndCheckAsExpected(false, 1, 5);
    }

    @Test
    public void applyWhenManyRowsAndManyColumnsAndWithoutHeading() {
        applyAndCheckAsExpected(false, 9, 3);
    }

    @Test
    public void applyWhenOneRowAndOneColumnAndWithHeading() {
        applyAndCheckAsExpected(true, 1, 1);
    }

    @Test
    public void applyWhenManyRowsAndOneColumnAndWithHeading() {
        applyAndCheckAsExpected(true, 10, 1);
    }

    @Test
    public void applyWhenOneRowAndManyColumnsAndWithHeading() {
        applyAndCheckAsExpected(true, 1, 5);
    }

    @Test
    public void applyWhenManyRowsAndManyColumnsAndWithHeading() {
        applyAndCheckAsExpected(true, 9, 3);
    }

    @Test
    public void applyWhenValueIsNull() {
        final Writer wrappedWriter = new StringWriter();
        final TabularResultWriter resultWriter = new CsvStringTabularResultWriter(wrappedWriter);
        final TabularResult result = new DefaultTabularResult();
        result.addRow("columnName", null);

        final int rows = resultWriter.apply(result);
        assertThat(rows, is(1));
    }

    private void applyAndCheckAsExpected(final boolean outputHeading,
                                         final int numberOfRows,
                                         final int numberOfColumns) {

        final StringBuilder sb = new StringBuilder();

        if (outputHeading) {
            for (int columnNumber = 0; columnNumber < numberOfColumns; columnNumber++) {
                sb.append(format(KNOWN_KEY_PREFIX, columnNumber));

                if (columnNumber < (numberOfColumns - 1)) {
                    sb.append(DELIMITER);
                }
            }

            sb.append(NEW_LINE);
        }

        for (int rowNumber = 0; rowNumber < numberOfRows; rowNumber++) {
            for (int columnNumber = 0; columnNumber < numberOfColumns; columnNumber++) {
                sb.append(format(KNOWN_VALUE_PREFIX, rowNumber, columnNumber));

                if (columnNumber < (numberOfColumns - 1)) {
                    sb.append(DELIMITER);
                }
            }

            sb.append(NEW_LINE);
        }

        final String expectedOutput = sb.toString();
        final Writer wrappedWriter = new StringWriter();
        final TabularResultWriter resultWriter = new CsvStringTabularResultWriter(wrappedWriter, outputHeading);

        final int rows = resultWriter.apply(getPopulatedResult(numberOfRows, numberOfColumns));
        assertThat(rows, is(numberOfRows));

        final String output = wrappedWriter.toString();
        assertThat(output, is(expectedOutput));
        assertThat(output.length(), is(expectedOutput.length()));
    }

    private static TabularResult getPopulatedResult(final int numberOfRows,
                                                    final int numberOfColumns) {

        final TabularResult result = new DefaultTabularResult();

        for (int rowNumber = 0; rowNumber < numberOfRows; rowNumber++) {
            final Map<String, Object> row = new LinkedHashMap<String, Object>();

            for (int columnNumber = 0; columnNumber < numberOfColumns; columnNumber++) {
                final String columnName = format(KNOWN_KEY_PREFIX, columnNumber);
                final String value = format(KNOWN_VALUE_PREFIX, rowNumber, columnNumber);

                row.put(columnName, value);
            }

            result.addRow(row);
        }

        return result;
    }
}
