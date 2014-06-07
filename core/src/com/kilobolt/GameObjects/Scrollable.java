package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Vector2;

public class Scrollable {
    private Vector2 position;
    private Vector2 velocity;
    private final int myWidth;
    private int myHeight;
    private boolean objectIsScrollableRight;

    public Scrollable(float x, float y, int width, int height,
            float scrollSpeed) {
        position = new Vector2(x, y);
        velocity = new Vector2(scrollSpeed, 0);
        myWidth = width;
        myHeight = height;
        objectIsScrollableRight = false;
    }

    public void update(float delta) {
        position.add(velocity.cpy().scl(delta));
        if (objectHasScrolledOffScreen()) {
            objectIsScrollableRight = true;
        }
    }

    private boolean objectHasScrolledOffScreen() {
        return position.x + myWidth <= 0;
    }

    protected void reset(float newX) {
        position.x = newX;
        objectIsScrollableRight = false;
    }

    public final void stop() {
        velocity.x = 0;
    }

    public final boolean objectIsScrollableRight() {
        return objectIsScrollableRight;
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

    protected final void setPositionX(float x) {
        this.position.x = x;
    }

    protected final void setVelocityX(float x) {
        this.velocity.x = x;
    }

    public final void setHeight(int height) {
        myHeight = height;
    }
}
