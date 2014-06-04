package com.kilobolt.GameObjects;

import java.util.Random;

import com.badlogic.gdx.math.Circle;
import com.badlogic.gdx.math.Intersector;
import com.badlogic.gdx.math.Rectangle;

public final class Pipe extends Scrollable {
    private static final int VERTICAL_PIPE_GAP = 45;
    private static final int SKULL_WIDTH = 24;
    private static final int SKULL_HEIGHT = 11;
    private final float myGroundY;
    private boolean isScored = false;
    private final Random random;
    private final Rectangle skullUp, skullDown, barUp, barDown;

    public Pipe(float x, float y, int width, int height, float scrollSpeed,
            float groundY) {
        super(x, y, width, height, scrollSpeed);
        random = new Random();
        skullUp = new Rectangle();
        skullDown = new Rectangle();
        barUp = new Rectangle();
        barDown = new Rectangle();
        myGroundY = groundY;
    }

    @Override
    public void update(float delta) {
        super.update(delta);
        updateBarCollisionBox();
        updateSkullCollisionBox();
    }

    private void updateBarCollisionBox() {
        barUp.set(getX(), getY(), getWidth(), getHeight());
        barDown.set(getX(), getY() + getHeight() + VERTICAL_PIPE_GAP,
                getWidth(),
                myGroundY - (getY() + getHeight() + VERTICAL_PIPE_GAP));
    }

    private void updateSkullCollisionBox() {
        skullUp.set(getX() - (SKULL_WIDTH - getWidth()) / 2, getY()
                + getHeight()
                - SKULL_HEIGHT, SKULL_WIDTH, SKULL_HEIGHT);
        skullDown.set(getX() - (SKULL_WIDTH - getWidth()) / 2, barDown.y,
                SKULL_WIDTH, SKULL_HEIGHT);
    }

    @Override
    public void reset(float newX) {
        super.reset(newX);
        setHeight(random.nextInt(90) + 15);
        isScored = false;
    }

    public boolean hasCollided(Bird bird) {
        if (birdHasCrossedAPipe(bird)) {
            return hasBirdCollidedWithPipe(bird);
        }
        return false;
    }

    private boolean birdHasCrossedAPipe(Bird bird) {
        return getX() < bird.getX() + bird.getWidth();
    }

    private boolean hasBirdCollidedWithPipe(Bird bird) {
        final Circle c = bird.getCollisionCircle();
        return (Intersector.overlaps(c, barUp)
                || Intersector.overlaps(c, barDown)
                || Intersector.overlaps(c, skullUp)
                || Intersector.overlaps(c, skullDown));
    }

    public void onRestart(float x, float scrollSpeed) {
        setVelocityX(scrollSpeed);
        reset(x);
    }

    public Rectangle getSkullUp() {
        return skullUp;
    }

    public Rectangle getSkullDown() {
        return skullDown;
    }

    public Rectangle getBarUp() {
        return barUp;
    }

    public Rectangle getBarDown() {
        return barDown;
    }

    public boolean isScored() {
        return isScored;
    }

    public void setScored(boolean b) {
        isScored = b;
    }
}
