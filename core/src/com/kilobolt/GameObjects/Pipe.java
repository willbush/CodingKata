package com.kilobolt.GameObjects;

import java.util.Random;

import com.badlogic.gdx.math.Circle;
import com.badlogic.gdx.math.Intersector;
import com.badlogic.gdx.math.Rectangle;

public class Pipe extends Scrollable {
    private static final int VERTICAL_PIPE_GAP = 45;
    private static final int SKULL_WIDTH = 24;
    private static final int SKULL_HEIGHT = 11;
    private final float groundY;
    private final Random r;
    private final Rectangle skullUp, skullDown, barUp, barDown;

    public Pipe(float x, float y, int width, int height, float scrollSpeed,
            float groundY) {
        super(x, y, width, height, scrollSpeed);
        r = new Random();
        skullUp = new Rectangle();
        skullDown = new Rectangle();
        barUp = new Rectangle();
        barDown = new Rectangle();
        this.groundY = groundY;
    }

    @Override
    public void update(float delta) {
        super.update(delta);
        updateBarCollisionBox();
        updateSkullCollisionBox();
    }

    private void updateBarCollisionBox() {
        barUp.set(position.x, position.y, width, height);
        barDown.set(position.x, position.y + height + VERTICAL_PIPE_GAP, width,
                groundY - (position.y + height + VERTICAL_PIPE_GAP));
    }

    private void updateSkullCollisionBox() {
        skullUp.set(position.x - (SKULL_WIDTH - width) / 2, position.y + height
                - SKULL_HEIGHT, SKULL_WIDTH, SKULL_HEIGHT);
        skullDown.set(position.x - (SKULL_WIDTH - width) / 2, barDown.y,
                SKULL_WIDTH, SKULL_HEIGHT);
    }

    @Override
    public void reset(float newX) {
        super.reset(newX);
        height = r.nextInt(90) + 15;
    }

    public boolean collides(Bird bird) {
        if (birdHasCrossedAPipe(bird))
            return birdHasCollidedWithPipe(bird);
        return false;
    }

    private boolean birdHasCrossedAPipe(Bird bird) {
        return position.x < bird.getX() + bird.getWidth();
    }

    private boolean birdHasCollidedWithPipe(Bird bird) {
        final Circle c = bird.getCollisionCircle();
        return (Intersector.overlaps(c, barUp)
                || Intersector.overlaps(c, barDown)
                || Intersector.overlaps(c, skullUp)
                || Intersector.overlaps(c, skullDown));
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
}
