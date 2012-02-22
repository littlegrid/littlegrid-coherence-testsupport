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

package org.littlegrid.support;

import org.junit.Test;

import java.util.Properties;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Bean utilities test.
 */
public final class BeanUtilsTest {
    private static final String NAME_PROPERTY = "Name";
    private static final String AGE_PROPERTY = "Age";
    private static final String BIRTH_DATE_MILLIS_PROPERTY = "BirthDateMillis";
    private static final String EXPECTED_NAME = "Fred Bloggs";
    private static final int EXPECTED_AGE = 30;
    private static final long EXPECTED_BIRTH_DATE_MILLIS = 1234567890;


    @Test
    public void emptyPropertiesNothingToSet() {
        Properties properties = new Properties();
        Person person = new Person();

        int propertiesSetCount = BeanUtils.multiSetter(person, properties);

        assertThat(propertiesSetCount, is(0));
        assertThat(person.getName(), nullValue());
        assertThat(person.getAge(), is(0));
        assertThat(person.getBirthDateMillis(), is(0L));
    }

    @Test
    public void setWhenValueIsString() {
        Properties properties = new Properties();
        properties.setProperty(NAME_PROPERTY, EXPECTED_NAME);

        Person person = new Person();

        int propertiesSetCount = BeanUtils.multiSetter(person, properties);

        assertThat(propertiesSetCount, is(1));
        assertThat(person.getName(), is(EXPECTED_NAME));
        assertThat(person.getAge(), is(0));
        assertThat(person.getBirthDateMillis(), is(0L));
    }

    @Test
    public void setWhenValueIsInteger() {
        Properties properties = new Properties();
        properties.setProperty(AGE_PROPERTY, Integer.toString(EXPECTED_AGE));

        Person person = new Person();

        int propertiesSetCount = BeanUtils.multiSetter(person, properties);

        assertThat(propertiesSetCount, is(1));
        assertThat(person.getAge(), is(EXPECTED_AGE));
        assertThat(person.getBirthDateMillis(), is(0L));
    }

    @Test
    public void setWhenValueIsLong() {
        Properties properties = new Properties();
        properties.setProperty(BIRTH_DATE_MILLIS_PROPERTY, Long.toString(EXPECTED_BIRTH_DATE_MILLIS));

        Person person = new Person();

        int propertiesSetCount = BeanUtils.multiSetter(person, properties);

        assertThat(propertiesSetCount, is(1));
        assertThat(person.getAge(), is(0));
        assertThat(person.getBirthDateMillis(), is(EXPECTED_BIRTH_DATE_MILLIS));
    }

    @Test
    public void setAll() {
        Properties properties = new Properties();
        properties.setProperty(NAME_PROPERTY, EXPECTED_NAME);
        properties.setProperty(AGE_PROPERTY, Integer.toString(EXPECTED_AGE));
        properties.setProperty(BIRTH_DATE_MILLIS_PROPERTY, Long.toString(EXPECTED_BIRTH_DATE_MILLIS));

        Person person = new Person();

        int propertiesSetCount = BeanUtils.multiSetter(person, properties);

        assertThat(propertiesSetCount, is(3));
        assertThat(person.getName(), is(EXPECTED_NAME));
        assertThat(person.getAge(), is(EXPECTED_AGE));
        assertThat(person.getBirthDateMillis(), is(EXPECTED_BIRTH_DATE_MILLIS));
    }

    @Test(expected = IllegalStateException.class)
    public void setWhenPropertyDoesNotExist() {
        Properties properties = new Properties();
        properties.setProperty("NonExistentProperty", "DoesNotMatterPropertyDoesNotExist");

        BeanUtils.multiSetter(new Person(), properties);
    }


    public static final class Person {
        private String name;
        private int age;
        private long birthDateMillis;

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

        public void setBirthDateMillis(final long birthDateMillis) {
            this.birthDateMillis = birthDateMillis;
        }

        public long getBirthDateMillis() {
            return birthDateMillis;
        }

        /*
           Required for older Coherence versions, such as 3.5.x
        */
        public void setAge(final String age) {
            setAge(Integer.parseInt(age));
        }
    }
}
