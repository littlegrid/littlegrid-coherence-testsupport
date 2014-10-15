package org.littlegrid.management.impl;

import org.junit.Test;

/**
 * Update parser tests.
 */
public class DefaultUpdateParserTest {
    @Test
    public void whatever() {
        final UpdateParser parser = new DefaultUpdateParser("update x set a = 'a1', b = 'b1' where c = 'some value'");
    }
}
