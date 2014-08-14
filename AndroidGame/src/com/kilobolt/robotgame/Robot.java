package com.kilobolt.robotgame;

import java.util.ArrayList;

import android.graphics.Rect;

public class Robot {

    public static Rect rect = new Rect(0, 0, 0, 0);
    public static Rect rect2 = new Rect(0, 0, 0, 0);
    public static Rect rect3 = new Rect(0, 0, 0, 0);
    public static Rect rect4 = new Rect(0, 0, 0, 0);
    public static Rect checkForCollisionArea = new Rect(0, 0, 0, 0);

    public static Rect footleft = new Rect(0, 0, 0, 0);
    public static Rect footright = new Rect(0, 0, 0, 0);

    private static final int JUMP_SPEED = -15;
    private static final int MOVE_SPEED = 5;
    private static final int START_SCROLLING_POS = 200;

    private int centerX = 100;
    private int centerY = 377;
    private boolean jumped = false;
    private boolean movingLeft = false;
    private boolean movingRight = false;
    private boolean ducked = false;
    private boolean readyToFire = true;

    private int speedX = 0;
    private int speedY = 0;

    private Background bg1 = GameScreen.getBg1();
    private Background bg2 = GameScreen.getBg2();

    private ArrayList<Projectile> projectiles = new ArrayList<Projectile>();

    public void update() {
        handleWalking();
        handleBackgroundScrolling();
        handleJumping();
        preventRobotMovingOutOfFrame();
        robotCollisionBoxes();
        robotCheckColisionBox();
    }

    private void preventRobotMovingOutOfFrame() {
        if (centerX + speedX <= 60) {
            centerX = 61;
        }
    }

    private void handleJumping() {
        centerY += speedY;
        speedY += 1;
        if (speedY > 3) {
            jumped = true;
        }
    }

    private void handleBackgroundScrolling() {
        if (speedX == 0 || speedX < 0) {
            bg1.setSpeedX(0);
            bg2.setSpeedX(0);

        }
        if (speedX > 0 && centerX > START_SCROLLING_POS) {
            bg1.setSpeedX(-MOVE_SPEED / 5);
            bg2.setSpeedX(-MOVE_SPEED / 5);
        }
    }

    private void handleWalking() {
        if (speedX < 0) {
            centerX += speedX;
        }
        if (centerX <= START_SCROLLING_POS && speedX > 0) {
            centerX += speedX;
        }
    }

    private void robotCollisionBoxes() {
        rect.set(centerX - 34, centerY - 63, centerX + 34, centerY);
        rect2.set(rect.left, rect.top + 63, rect.left + 68, rect.top + 128);
        rect3.set(rect.left - 26, rect.top + 32, rect.left, rect.top + 52);
        rect4.set(rect.left + 68, rect.top + 32, rect.left + 94, rect.top + 52);
        footleft.set(centerX - 50, centerY + 20, centerX, centerY + 35);
        footright.set(centerX, centerY + 20, centerX + 50, centerY + 35);
    }

    private void robotCheckColisionBox() {
        checkForCollisionArea.set(centerX - 110, centerY - 110, centerX + 70,
                centerY + 70);
    }

    public void moveRight() {
        if (ducked == false) {
            speedX = MOVE_SPEED;
        }
    }

    public void moveLeft() {
        if (ducked == false) {
            speedX = -MOVE_SPEED;
        }
    }

    public void stopRight() {
        setMovingRight(false);
        stop();
    }

    public void stopLeft() {
        setMovingLeft(false);
        stop();
    }

    private void stop() {
        if (isMovingRight() == false && isMovingLeft() == false) {
            speedX = 0;
        }

        if (isMovingRight() == false && isMovingLeft() == true) {
            moveLeft();
        }

        if (isMovingRight() == true && isMovingLeft() == false) {
            moveRight();
        }
    }

    public void jump() {
        if (jumped == false) {
            speedY = JUMP_SPEED;
            jumped = true;
        }
    }

    public void shoot() {
        if (readyToFire) {
            Projectile p = new Projectile(centerX + 50, centerY - 25);
            projectiles.add(p);
        }
    }

    public int getCenterX() {
        return centerX;
    }

    public int getCenterY() {
        return centerY;
    }

    public boolean isJumped() {
        return jumped;
    }

    public int getSpeedX() {
        return speedX;
    }

    public int getSpeedY() {
        return speedY;
    }

    public void setCenterX(int centerX) {
        this.centerX = centerX;
    }

    public void setCenterY(int centerY) {
        this.centerY = centerY;
    }

    public void setJumped(boolean jumped) {
        this.jumped = jumped;
    }

    public void setSpeedX(int speedX) {
        this.speedX = speedX;
    }

    public void setSpeedY(int speedY) {
        this.speedY = speedY;
    }

    public boolean isDucked() {
        return ducked;
    }

    public void setDucked(boolean ducked) {
        this.ducked = ducked;
    }

    public boolean isMovingRight() {
        return movingRight;
    }

    public void setMovingRight(boolean movingRight) {
        this.movingRight = movingRight;
    }

    public boolean isMovingLeft() {
        return movingLeft;
    }

    public void setMovingLeft(boolean movingLeft) {
        this.movingLeft = movingLeft;
    }

    public ArrayList<Projectile> getProjectiles() {
        return projectiles;
    }

    public boolean isReadyToFire() {
        return readyToFire;
    }

    public void setReadyToFire(boolean readyToFire) {
        this.readyToFire = readyToFire;
    }
}
