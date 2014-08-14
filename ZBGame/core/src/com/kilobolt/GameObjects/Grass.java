package com.kilobolt.GameObjects;

public final class Grass extends Scrollable {
    public Grass(float x, float y, int width, int height, float scrollSpeed) {
        super(x, y, width, height, scrollSpeed);
    }

    public void onRestart(float x, float scrollSpeed) {
        setPositionX(x);
        setVelocityX(scrollSpeed);
    }
}
