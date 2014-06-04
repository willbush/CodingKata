package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Circle;
import com.badlogic.gdx.math.Vector2;
import com.kilobolt.ZBHelpers.AssetLoader;

public final class Bird {
    private static final int TERMINAL_VELOCITY = 200;
    private static final int GRAVITY = 450;
    private final Vector2 position;
    private final Vector2 velocity;
    private final Vector2 acceleration;
    private float rotation;
    private final int height;
    private final int width;
    private final float startingY;
    private boolean isAlive;
    private final Circle collisionCircle;

    public Bird(float x, float y, int width, int height) {
        this.width = width;
        this.height = height;
        startingY = y;
        position = new Vector2(x, y);
        velocity = new Vector2(0, 0);
        acceleration = new Vector2(0, GRAVITY);
        collisionCircle = new Circle();
        isAlive = true;
    }

    public void updateReady(float runTime) {
        position.y = 2 * (float) Math.sin(7 * runTime) + startingY;
    }

    public void update(float delta) {
        updateVelocity(delta);
        updatePosition(delta);
        updateCollisionCircle();
        updateRotation(delta);
        checkCeilingCollision();
    }

    private void updateVelocity(float delta) {
        velocity.add(acceleration.cpy().scl(delta));
        if (velocity.y > TERMINAL_VELOCITY) {
            velocity.y = TERMINAL_VELOCITY;
        }
    }

    private void updatePosition(float delta) {
        position.add(velocity.cpy().scl(delta));
    }

    private void updateCollisionCircle() {
        final int shiftX = 9;
        final int shiftY = 6;
        final float radius = 6.5f;
        collisionCircle.set(position.x + shiftX, position.y + shiftY, radius);
    }

    private void updateRotation(float delta) {
        if (isRising()) {
            rotateCounterclockwise(delta);
        }
        if (isFalling()) {
            rotateClockwise(delta);
        }
    }

    private void rotateClockwise(float delta) {
        rotation += 480 * delta;
        final int maxClockwiseRotation = 90;
        if (rotation > maxClockwiseRotation) {
            rotation = maxClockwiseRotation;
        }
    }

    private void rotateCounterclockwise(float delta) {
        rotation -= 600 * delta;
        final int maxCounterRotation = -20;
        if (rotation < maxCounterRotation) {
            rotation = maxCounterRotation;
        }
    }

    private void checkCeilingCollision() {
        final int ceiling = -13;
        if (position.y < ceiling) {
            position.y = ceiling;
            velocity.y = 0;
        }
    }

    public void die() {
        isAlive = false;
        velocity.y = 0;
    }

    public void decelerate() {
        acceleration.y = 0;
    }

    public void onRestart(int y) {
        rotation = 0;
        position.y = y;
        velocity.x = 0;
        velocity.y = 0;
        acceleration.x = 0;
        acceleration.y = GRAVITY;
        isAlive = true;
    }

    public void onClick() {
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

    public boolean isFalling() {
        final int fallingThreshold = 110;
        return velocity.y > fallingThreshold;
    }

    public boolean isRising() {
        final int risingThreshold = 0;
        return velocity.y < risingThreshold;
    }

    public boolean isFlapping() {
        final int flappingThreshold = 70;
        return (velocity.y <= flappingThreshold) && isAlive;
    }

    public boolean isAlive() {
        return isAlive;
    }

    public float getX() {
        return position.x;
    }

    public float getY() {
        return position.y;
    }

    public float getRotation() {
        return rotation;
    }

    public int getHeight() {
        return height;
    }

    public int getWidth() {
        return width;
    }

    public Circle getCollisionCircle() {
        return collisionCircle;
    }
}
