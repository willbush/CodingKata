package com.kilobolt.GameObjects;

import com.badlogic.gdx.math.Vector2;

public class Bird {

    private Vector2 position;
    private Vector2 velocity;
    private Vector2 acceleration;

    private float rotation;
    private int height;
    private int width;

    private static final int TERMINAL_VELOCITY = 200;

    public Bird(float x, float y, int width, int height) {
        this.width = width;
        this.height = height;
        position = new Vector2(x, y);
        acceleration = new Vector2(0, 240);
    }

    public void update(float delta) {
        velocity.add(acceleration.cpy().scl(delta));

        if (velocity.y > TERMINAL_VELOCITY)
            velocity.y = TERMINAL_VELOCITY;

        position.add(velocity.cpy().scl(delta));
    }

    public void onClick() {
        velocity.y = -140;
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
