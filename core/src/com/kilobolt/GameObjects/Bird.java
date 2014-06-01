package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Circle;
import com.badlogic.gdx.math.Vector2;
import com.kilobolt.ZBHelpers.AssetLoader;

public class Bird {
    private final Vector2 position;
    private final Vector2 velocity;
    private final Vector2 acceleration;
    private float rotation;
    private boolean isAlive = true;
    private final int myHeight;
    private final int myWidth;
    private final Circle collisionCircle;
    private static final int TERMINAL_VELOCITY = 200;

    public Bird(final float x, final float y, final int width,
            final int height) {
        myWidth = width;
        myHeight = height;
        position = new Vector2(x, y);
        velocity = new Vector2(0, 0);
        acceleration = new Vector2(0, 240);
        collisionCircle = new Circle();
    }

    public final void update(final float delta) {
        updateVelocity(delta);
        updatePosition(delta);
        updateCollisionCircle();
        updateRotation(delta);
    }

    private void updateVelocity(final float delta) {
        velocity.add(acceleration.cpy().scl(delta));
        if (velocity.y > TERMINAL_VELOCITY) {
            velocity.y = TERMINAL_VELOCITY;
        }
    }

    private void updatePosition(final float delta) {
        position.add(velocity.cpy().scl(delta));
    }

    private void updateCollisionCircle() {
        collisionCircle.set(position.x + 9, position.y + 6, 6.5f);
    }

    private void updateRotation(final float delta) {
        if (isRising()) {
            rotateCounterclockwise(delta);
        }
        if (isFalling()) {
            rotateClockwise(delta);
        }
    }

    private void rotateClockwise(final float delta) {
        rotation += 480 * delta;
        final int maxClockwiseRotation = 90;
        if (rotation > maxClockwiseRotation) {
            rotation = maxClockwiseRotation;
        }
    }

    private void rotateCounterclockwise(final float delta) {
        rotation -= 600 * delta;
        final int maxCounterRotation = -20;
        if (rotation < maxCounterRotation) {
            rotation = maxCounterRotation;
        }
    }

    public final void die() {
        isAlive = false;
        velocity.y = 0;
    }

    public final void decelerate() {
        acceleration.y = 0;
    }

    public final void onClick() {
        if (isAlive) {
            flapWings();
        }
    }

    private void flapWings() {
        if (isAlive) {
            final int flappingVelocity = -140;
            velocity.y = flappingVelocity;
            AssetLoader.getFlap().play();
        }
    }

    public final boolean isFalling() {
        final int fallingThreshold = 110;
        return velocity.y > fallingThreshold;
    }

    public final boolean isRising() {
        final int risingThreshold = 0;
        return velocity.y < risingThreshold;
    }

    public final boolean isFlapping() {
        final int flappingThreshold = 70;
        return (velocity.y <= flappingThreshold) && isAlive;
    }

    public final boolean isAlive() {
        return isAlive;
    }

    public final float getX() {
        return position.x;
    }

    public final float getY() {
        return position.y;
    }

    public final float getRotation() {
        return rotation;
    }

    public final int getHeight() {
        return myHeight;
    }

    public final int getWidth() {
        return myWidth;
    }

    public final Circle getCollisionCircle() {
        return collisionCircle;
    }
}
