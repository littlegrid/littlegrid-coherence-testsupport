package org.littlegrid.utils;

import org.junit.Test;
import org.littlegrid.utils.PropertiesUtils;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Properties utilities tests.
 */
public final class PropertiesUtilsTest {

    public static final int EXPECTED_SIZE_OF_TWO_PROPERTIES_FILE_COMBINED = 7;
    public static final int EXPECTED_SIZE_OF_SINGLE_PROPERTIES_FILE = 6;
    public static final String FILE_THAT_DOES_NOT_EXIST = "this-file-does-not-exist.properties";
    public static final String FILE_THAT_DOES_EXIST = "properties/memberGroup1.properties";
    public static final String ANOTHER_FILE_THAT_DOES_EXIST = "properties/memberGroup2.properties";

    @Test
    public void fileThatDoesNotExist() {
        final String[] propertiesFilenames = {FILE_THAT_DOES_NOT_EXIST};

        final Properties properties = PropertiesUtils.loadProperties(propertiesFilenames);

        assertThat(properties.size(), is(0));
    }

    @Test
    public void fileThatExists() {
        final String[] propertiesFilename = {FILE_THAT_DOES_EXIST};

        final Properties properties = PropertiesUtils.loadProperties(propertiesFilename);

        assertThat(properties.size(), is(EXPECTED_SIZE_OF_SINGLE_PROPERTIES_FILE));
    }

    @Test
    public void fileThatExistsButHasSpacesEitherSide() {
        final String[] propertiesFilename = {" " + FILE_THAT_DOES_EXIST + " "};

        final Properties properties = PropertiesUtils.loadProperties(propertiesFilename);

        assertThat(properties.size(), is(EXPECTED_SIZE_OF_SINGLE_PROPERTIES_FILE));
    }

    @Test
    public void fileThatDoesNotExistAndFileThatDoesExist() {
        final String[] propertiesFilename = {FILE_THAT_DOES_NOT_EXIST, FILE_THAT_DOES_EXIST};

        final Properties properties = PropertiesUtils.loadProperties(propertiesFilename);

        assertThat(properties.size(), is(EXPECTED_SIZE_OF_SINGLE_PROPERTIES_FILE));
    }

    @Test
    public void twoFilesThatExistWithOverridingKeys() {
        final String[] propertiesFilenames = {
                FILE_THAT_DOES_EXIST,
                ANOTHER_FILE_THAT_DOES_EXIST
        };

        final Properties properties = PropertiesUtils.loadProperties(propertiesFilenames);

        assertThat(properties.size(), is(EXPECTED_SIZE_OF_TWO_PROPERTIES_FILE_COMBINED));
    }

    @Test
    public void twoFilesUsingCommaDelimitedStringWithOverridingKeys() {
        final String commaDelimitedPropertiesFilenames =
                FILE_THAT_DOES_EXIST + ", " + ANOTHER_FILE_THAT_DOES_EXIST;

        final Properties properties = PropertiesUtils.loadProperties(commaDelimitedPropertiesFilenames);

        assertThat(properties.size(), is(EXPECTED_SIZE_OF_TWO_PROPERTIES_FILE_COMBINED));
    }
}
