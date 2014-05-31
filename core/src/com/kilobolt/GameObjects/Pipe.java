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
    private final Random r;
    private final Rectangle skullUp, skullDown, barUp, barDown;

    public Pipe(final float x, final float y, final int width,
            final int height, final float scrollSpeed,
            final float groundY) {
        super(x, y, width, height, scrollSpeed);
        r = new Random();
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
        barUp.set(position.x, position.y, width, height);
        barDown.set(position.x, position.y + height + VERTICAL_PIPE_GAP, width,
                myGroundY - (position.y + height + VERTICAL_PIPE_GAP));
    }

    private void updateSkullCollisionBox() {
        skullUp.set(position.x - (SKULL_WIDTH - width) / 2, position.y + height
                - SKULL_HEIGHT, SKULL_WIDTH, SKULL_HEIGHT);
        skullDown.set(position.x - (SKULL_WIDTH - width) / 2, barDown.y,
                SKULL_WIDTH, SKULL_HEIGHT);
    }

    @Override
    public final void reset(final float newX) {
        super.reset(newX);
        height = r.nextInt(90) + 15;
    }

    public final boolean collides(final Bird bird) {
        if (birdHasCrossedAPipe(bird)) {
            return birdHasCollidedWithPipe(bird);
        }
        return false;
    }

    private boolean birdHasCrossedAPipe(final Bird bird) {
        return position.x < bird.getX() + bird.getWidth();
    }

    private boolean birdHasCollidedWithPipe(final Bird bird) {
        final Circle c = bird.getCollisionCircle();
        return (Intersector.overlaps(c, barUp)
                || Intersector.overlaps(c, barDown)
                || Intersector.overlaps(c, skullUp)
                || Intersector.overlaps(c, skullDown));
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
}
