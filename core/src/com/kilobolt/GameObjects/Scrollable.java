package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Vector2;

public class Scrollable {
    private final Vector2 position;
    private final Vector2 velocity;
    private final int myWidth;
    private int myHeight;
    private boolean isScrolledLeft;

    public Scrollable(final float x, final float y, final int width,
            final int height, final float scrollSpeed) {
        position = new Vector2(x, y);
        velocity = new Vector2(scrollSpeed, 0);
        myWidth = width;
        myHeight = height;
        isScrolledLeft = false;
    }

    public void update(final float delta) {
        position.add(velocity.cpy().scl(delta));
        if (!objectIsVisible()) {
            isScrolledLeft = true;
        }
    }

    private boolean objectIsVisible() {
        return position.x + myWidth >= 0;
    }

    protected void reset(final float newX) {
        position.x = newX;
        isScrolledLeft = false;
    }

    public final void stop() {
        velocity.x = 0;
    }

    public final boolean isScrolledLeft() {
        return isScrolledLeft;
    }

    public final float getTailX() {
        return position.x + myWidth;
    }

    public final float getX() {
        return position.x;
    }

    public final float getY() {
        return position.y;
    }

    public final int getWidth() {
        return myWidth;
    }

    public final int getHeight() {
        return myHeight;
    }

    public final void setHeight(final int height) {
        myHeight = height;
    }
}
