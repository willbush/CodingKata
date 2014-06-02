package com.kilobolt.GameObjects;

import java.util.Random;

import com.badlogic.gdx.math.Circle;
import com.badlogic.gdx.math.Intersector;
import com.badlogic.gdx.math.Rectangle;

public class Pipe extends Scrollable {
    private static final int VERTICAL_PIPE_GAP = 45;
    private static final int SKULL_WIDTH = 24;
    private static final int SKULL_HEIGHT = 11;
    private final float myGroundY;
    private boolean isScored = false;
    private final Random random;
    private final Rectangle skullUp, skullDown, barUp, barDown;

    public Pipe(final float x, final float y, final int width,
            final int height, final float scrollSpeed,
            final float groundY) {
        super(x, y, width, height, scrollSpeed);
        random = new Random();
        skullUp = new Rectangle();
        skullDown = new Rectangle();
        barUp = new Rectangle();
        barDown = new Rectangle();
        myGroundY = groundY;
    }

    @Override
    public final void update(final float delta) {
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
    public final void reset(final float newX) {
        super.reset(newX);
        setHeight(random.nextInt(90) + 15);
        isScored = false;
    }

    public final boolean hasCollided(final Bird bird) {
        if (birdHasCrossedAPipe(bird)) {
            return hasBirdCollidedWithPipe(bird);
        }
        return false;
    }

    private boolean birdHasCrossedAPipe(final Bird bird) {
        return getX() < bird.getX() + bird.getWidth();
    }

    private boolean hasBirdCollidedWithPipe(final Bird bird) {
        final Circle c = bird.getCollisionCircle();
        return (Intersector.overlaps(c, barUp)
                || Intersector.overlaps(c, barDown)
                || Intersector.overlaps(c, skullUp)
                || Intersector.overlaps(c, skullDown));
    }

    public final void onRestart(final float x, final float scrollSpeed) {
        setVelocityX(scrollSpeed);
        reset(x);
    }

    public final Rectangle getSkullUp() {
        return skullUp;
    }

    public final Rectangle getSkullDown() {
        return skullDown;
    }

    public final Rectangle getBarUp() {
        return barUp;
    }

    public final Rectangle getBarDown() {
        return barDown;
    }

    public final boolean isScored() {
        return isScored;
    }

    public final void setScored(final boolean b) {
        isScored = b;
    }
}
