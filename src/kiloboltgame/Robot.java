package kiloboltgame;

public class Robot {
    private int centerX = 100;
    private int centerY = 382;
    private int speedX = 0;
    private int speedY = 1;
    private boolean jumped = false;

    public void moveRight() {
        speedX = 6;
    }

    public void moveLeft() {
        speedX = -6;
    }

    public void stop() {
        speedX = 0;
    }

    public void jump() {
        if (jumped == false) {
            speedY = -15;
            jumped = true;
        }
    }

    public int getCenterX() {
        return centerX;
    }

    public int getCenterY() {
        return centerY;
    }

    public int getSpeedX() {
        return speedX;
    }

    public int getSpeedY() {
        return speedY;
    }

    public boolean isJumped() {
        return jumped;
    }

    public void setCenterX(int centerX) {
        this.centerX = centerX;
    }

    public void setCenterY(int centerY) {
        this.centerY = centerY;
    }

    public void setSpeedX(int speedX) {
        this.speedX = speedX;
    }

    public void setSpeedY(int speedY) {
        this.speedY = speedY;
    }

    public void setJumped(boolean jumped) {
        this.jumped = jumped;
    }

    public void update() {
        moveCharOrScrollBackground();
        updateYPosition();
        handleJumping();
        preventXCordTraversingZero();
    }

    private void moveCharOrScrollBackground() {
        if (speedX < 0) {
            centerX += speedX;
        } else if (speedX == 0) {
            System.out.println("Do not scroll the background");
        } else {
            if (centerX <= 150) {
                centerX += speedX;
            } else {
                System.out.println("Scroll background here.");
            }
        }
    }

    private void updateYPosition() {
        if (centerY + speedY >= 382) {
            centerY = 382;
        } else {
            centerY += speedY;
        }
    }

    private void handleJumping() {
        if (jumped) {
            speedY += 1;
            if (centerY + speedY >= 382) {
                centerY = 382;
                speedY = 0;
                jumped = false;
            }
        }
    }

    private void preventXCordTraversingZero() {
        if (centerX + speedX <= 60) {
            centerX = 61;
        }
    }
}
