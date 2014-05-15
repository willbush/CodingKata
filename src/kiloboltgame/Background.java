package kiloboltgame;

public class Background {
    private int backgroundX, backgroundY, speedX;

    public Background(int x, int y) {
        backgroundX = x;
        backgroundY = y;
        speedX = 0;
    }

    public void update() {
        backgroundX += speedX;

        if (backgroundX <= -2160)
            backgroundX += 4320;
    }

    public int getBackgroundX() {
        return backgroundX;
    }

    public int getBackgroundY() {
        return backgroundY;
    }

    public int getSpeedX() {
        return speedX;
    }

    public void setBackgroundX(int backgroundX) {
        this.backgroundX = backgroundX;
    }

    public void setBackgroundY(int backgroundY) {
        this.backgroundY = backgroundY;
    }

    public void setSpeedX(int speedX) {
        this.speedX = speedX;
    }
}
