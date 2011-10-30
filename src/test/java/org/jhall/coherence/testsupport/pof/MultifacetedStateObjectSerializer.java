package org.jhall.coherence.testsupport.pof;

import com.tangosol.io.pof.PofReader;
import com.tangosol.io.pof.PofSerializer;
import com.tangosol.io.pof.PofWriter;

import java.io.IOException;

/**
 * Multifaceted state object serializer.
 */
public class MultifacetedStateObjectSerializer implements PofSerializer {
    private static final int PRIMITIVE_INT = 0;
    private static final int STANDARD_STRING = 1;

    @Override
    public void serialize(PofWriter pofWriter, Object object)
            throws IOException {

        MultifacetedStateObject multifacetedStateObject = (MultifacetedStateObject) object;

        pofWriter.writeInt(PRIMITIVE_INT, multifacetedStateObject.getPrimitiveInt());
        pofWriter.writeString(STANDARD_STRING, multifacetedStateObject.getStandardString());
        pofWriter.writeRemainder(null);
    }

    @Override
    public Object deserialize(PofReader pofReader)
            throws IOException {

        MultifacetedStateObject multifacetedStateObject = new MultifacetedStateObject();

        multifacetedStateObject.setPrimitiveInt(pofReader.readInt(PRIMITIVE_INT));
        multifacetedStateObject.setStandardString(pofReader.readString(STANDARD_STRING));
        pofReader.readRemainder();

        return multifacetedStateObject;
    }
}
