package org.jhall.coherence.testsupport.pof;

import com.tangosol.io.WrapperBufferInput;
import com.tangosol.io.WrapperBufferOutput;
import com.tangosol.io.pof.PofContext;
import com.tangosol.util.Binary;
import com.tangosol.util.ExternalizableHelper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * POF utils.
 */
public class PofUtils {
    private static String POF_CONTEXT_CANNOT_BE_NULL = "POF context cannot be null";

    /**
     * Private constructor to prevent creation.
     */
    private PofUtils() {
    }

    public static byte[] objectToBytes(Object objectToSerialize,
                                       PofContext pofContext) {

        if (pofContext == null) {
            throw new IllegalArgumentException(POF_CONTEXT_CANNOT_BE_NULL);
        }

        if (objectToSerialize == null) {
            throw new IllegalArgumentException("Object to serialize cannot be null");
        }

        Binary binary = ExternalizableHelper.toBinary(objectToSerialize, pofContext);

        return binary.toByteArray();
    }

    public static Object bytesToObject(byte[] bytesToDeserialize,
                                       PofContext pofContext) {

        if (pofContext == null) {
            throw new IllegalArgumentException(POF_CONTEXT_CANNOT_BE_NULL);
        }

        if (bytesToDeserialize == null) {
            throw new IllegalArgumentException("Bytes to deserialize cannot be null");
        }

        Binary binary = new Binary(bytesToDeserialize);

        return ExternalizableHelper.fromBinary(binary, pofContext);
    }
}
