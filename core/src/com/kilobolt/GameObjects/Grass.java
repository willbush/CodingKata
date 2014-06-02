package com.kilobolt.GameObjects;

public class Grass extends Scrollable {
    public Grass(final float x, final float y, final int width,
            final int height, final float scrollSpeed) {
        super(x, y, width, height, scrollSpeed);
    }

    public final void onRestart(final float x, final float scrollSpeed) {
        setPositionX(x);
        setVelocityX(scrollSpeed);
    }
}
