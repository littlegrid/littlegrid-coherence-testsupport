package org.testsupport.common.utils;

import org.junit.Test;
import org.testsupport.common.AbstractTest;
import org.testsupport.common.utils.BeanUtils;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Bean utilities test.
 */
public class BeanUtilsTest extends AbstractTest {
    private static final String NAME_PROPERTY = "Name";
    private static final String AGE_PROPERTY = "Age";
    private static final String EXPECTED_NAME = "Fred Bloggs";
    private static final int EXPECTED_AGE = 30;


    @Test
    public void emptyPropertiesNothingToSet() {
        Properties properties = new Properties();
        Person person = new Person();

        int propertiesSetCount = BeanUtils.processProperties(person, properties);

        assertThat(propertiesSetCount, is(0));
        assertThat(person.getName(), nullValue());
        assertThat(person.getAge(), is(0));
    }

    @Test
    public void setWhenValueIsString() {
        Properties properties = new Properties();
        properties.setProperty(NAME_PROPERTY, EXPECTED_NAME);

        Person person = new Person();

        int propertiesSetCount = BeanUtils.processProperties(person, properties);

        assertThat(propertiesSetCount, is(1));
        assertThat(person.getName(), is(EXPECTED_NAME));
        assertThat(person.getAge(), is(0));
    }

    @Test
    public void setWhenValueIsNumber() {
        Properties properties = new Properties();
        properties.setProperty(AGE_PROPERTY, Integer.toString(EXPECTED_AGE));

        Person person = new Person();

        int propertiesSetCount = BeanUtils.processProperties(person, properties);

        assertThat(propertiesSetCount, is(1));
        assertThat(person.getAge(), is(EXPECTED_AGE));
    }

    @Test
    public void setAll() {
        Properties properties = new Properties();
        properties.setProperty(NAME_PROPERTY, EXPECTED_NAME);
        properties.setProperty(AGE_PROPERTY, Integer.toString(EXPECTED_AGE));

        Person person = new Person();

        int propertiesSetCount = BeanUtils.processProperties(person, properties);

        assertThat(propertiesSetCount, is(2));
        assertThat(person.getName(), is(EXPECTED_NAME));
        assertThat(person.getAge(), is(EXPECTED_AGE));
    }

    @Test(expected = RuntimeException.class)
    public void setWhenPropertyDoesNotExist() {
        Properties properties = new Properties();
        properties.setProperty("NonExistentProperty", "DoesNotMatterPropertyDoesNotExist");

        BeanUtils.processProperties(new Person(), properties);
    }


    public static class Person {
        private String name;
        private int age;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public int getAge() {
            return age;
        }

        public void setAge(final int age) {
            this.age = age;
        }
    }
}
