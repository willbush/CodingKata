/*
 * This class allows for floats to be tweened because only objects can be
 * tweened. In order to tween primative like a float, a class must be created.
 */
package com.kilobolt.TweenAccessors;

public final class Value {

    private float val = 1;

    public float getValue() {
        return val;
    }

    public void setValue(float newVal) {
        val = newVal;
    }

}
