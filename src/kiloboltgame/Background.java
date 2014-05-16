package kiloboltgame;

public class Background {
    private int bgX, bgY, bGspeedX;

    public Background(int x, int y) {
        bgX = x;
        bgY = y;
        bGspeedX = 0;
    }

    public void update() {
        bgX += bGspeedX;

        if (bgX <= -2160)
            bgX += 4320;
    }

    public int getBgX() {
        return bgX;
    }

    public int getBgY() {
        return bgY;
    }

    public int getBgSpeedX() {
        return bGspeedX;
    }

    public void setBackgroundX(int backgroundX) {
        this.bgX = backgroundX;
    }

    public void setBackgroundY(int backgroundY) {
        this.bgY = backgroundY;
    }

    public void setBgSpeedX(int speedX) {
        this.bGspeedX = speedX;
    }
}