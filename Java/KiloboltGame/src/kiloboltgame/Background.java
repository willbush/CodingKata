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
        recycleOutOfFrameBg();
    }

    private void recycleOutOfFrameBg() {
        if (bgX <= -2160)
            bgX += 4320;
    }

    public int getBgX() {
        return bgX;
    }

    public int getBgY() {
        return bgY;
    }

    public int getbGspeedX() {
        return bGspeedX;
    }

    public void setBgX(int bgX) {
        this.bgX = bgX;
    }

    public void setBgY(int bgY) {
        this.bgY = bgY;
    }

    public void setbGspeedX(int bGspeedX) {
        this.bGspeedX = bGspeedX;
    }
}