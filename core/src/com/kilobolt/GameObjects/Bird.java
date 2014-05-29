package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Vector2;

public class Bird {
    private final Vector2 position;
    private final Vector2 velocity;
    private final Vector2 acceleration;
    private float rotation;
    private final int height;
    private final int width;
    private static final int TERMINAL_VELOCITY = 200;

    public Bird(float x, float y, int width, int height) {
        this.width = width;
        this.height = height;
        position = new Vector2(x, y);
        velocity = new Vector2(0, 0);
        acceleration = new Vector2(0, 240);
    }

    public void update(float delta) {
        updateVelocity(delta);
        updatePosition(delta);
        updateRotation(delta);
    }

    private void updateRotation(float delta) {
        if (isRising())
            rotateCounterclockwise(delta);
        if (isFalling())
            rotateClockwise(delta);
    }

    private void rotateClockwise(float delta) {
        rotation += 480 * delta;
        final int maxClockwiseRotation = 90;
        if (rotation > maxClockwiseRotation)
            rotation = maxClockwiseRotation;
    }

    private void rotateCounterclockwise(float delta) {
        rotation -= 600 * delta;
        final int maxCounterRotation = -20;
        if (rotation < maxCounterRotation)
            rotation = maxCounterRotation;
    }

    private void updatePosition(float delta) {
        position.add(velocity.cpy().scl(delta));
    }

    private void updateVelocity(float delta) {
        velocity.add(acceleration.cpy().scl(delta));
        if (velocity.y > TERMINAL_VELOCITY)
            velocity.y = TERMINAL_VELOCITY;
    }

    public void onClick() {
        flapWings();
    }

    private void flapWings() {
        final int flappingVelocity = -140;
        velocity.y = flappingVelocity;
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
        return velocity.y < flappingThreshold;
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
}
