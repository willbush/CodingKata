package kiloboltgame;

public class Background {
    private int bgX, bgY, speedX;

    public Background(int x, int y) {
        bgX = x;
        bgY = y;
        speedX = 0;
    }

    public void update() {
        bgX += speedX;

        if (bgX <= -2160)
            bgX += 4320;
    }

    public int getBgX() {
        return bgX;
    }

    public int getBgY() {
        return bgY;
    }

    public int getSpeedX() {
        return speedX;
    }

    public void setBackgroundX(int backgroundX) {
        this.bgX = backgroundX;
    }

    public void setBackgroundY(int backgroundY) {
        this.bgY = backgroundY;
    }

    public void setSpeedX(int speedX) {
        this.speedX = speedX;
    }
}