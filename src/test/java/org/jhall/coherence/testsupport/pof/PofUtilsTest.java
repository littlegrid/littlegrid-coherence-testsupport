package org.jhall.coherence.testsupport.pof;

import com.tangosol.io.pof.ConfigurablePofContext;
import com.tangosol.io.pof.PofContext;
import org.jhall.coherence.testsupport.common.AbstractTestSupportTest;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.hamcrest.CoreMatchers.is;
import static org.jhall.coherence.testsupport.common.CommonTestSupportConst.POF_CONFIG_FILE;
import static org.jhall.coherence.testsupport.pof.PofUtils.bytesToObject;
import static org.jhall.coherence.testsupport.pof.PofUtils.objectToBytes;
import static org.junit.Assert.assertThat;

/**
 * POF utilities tests.
 */
public class PofUtilsTest extends AbstractTestSupportTest {
    private PofContext pofContext;

    @Before
    public void beforeTest() {
        pofContext = new ConfigurablePofContext(POF_CONFIG_FILE);
    }

    @Test
    public void objectToBytesAndBackForNonPopulatedObject()
            throws IOException {

        MultifacetedStateObject neverSerialized = new MultifacetedStateObject();

        byte[] bytes = objectToBytes(neverSerialized, pofContext);
        MultifacetedStateObject wasSerialized = (MultifacetedStateObject) bytesToObject(bytes, pofContext);

        checkAsExpected(neverSerialized, wasSerialized);
    }

    @Test
    public void objectToBytesAndBackForPopulatedObject()
            throws IOException {

        final int primitiveInt = 123;
        final String standardString = "abc";

        MultifacetedStateObject notSerialized = new MultifacetedStateObject();
        notSerialized.setPrimitiveInt(primitiveInt);
        notSerialized.setStandardString(standardString);

        byte[] bytes = objectToBytes(notSerialized, pofContext);
        MultifacetedStateObject wasSerialized = (MultifacetedStateObject) bytesToObject(bytes, pofContext);

        checkAsExpected(wasSerialized, notSerialized);
    }

    private void checkAsExpected(MultifacetedStateObject wasSerialized,
                                 MultifacetedStateObject notSerialized) {

        assertThat(wasSerialized.getPrimitiveInt(), is(notSerialized.getPrimitiveInt()));
        assertThat(wasSerialized.getStandardString(), is(notSerialized.getStandardString()));
    }
}
