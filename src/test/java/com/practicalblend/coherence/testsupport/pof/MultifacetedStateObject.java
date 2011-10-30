package com.practicalblend.coherence.testsupport.pof;

/**
 * Object containing lots of different types of state to be used for exercising POF.
 */
public class MultifacetedStateObject {
    //TODO: Expand the number and variety of types.
    private int primitiveInt;
    private String standardString;

    public int getPrimitiveInt() {
        return primitiveInt;
    }

    public void setPrimitiveInt(int primitiveInt) {
        this.primitiveInt = primitiveInt;
    }

    public String getStandardString() {
        return standardString;
    }

    public void setStandardString(String standardString) {
        this.standardString = standardString;
    }
}
