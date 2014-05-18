package kiloboltgame;

import java.awt.Image;

public class Tile {

    public Image tileImage;

    private int tileX, tileY, tileSpeedX, type;
    private Background bg = StartingClass.getBg1();

    public Tile(int x, int y, int typeInt) {
        tileX = x * 40;
        tileY = y * 40;

        type = typeInt;

        if (type == 5) {
            tileImage = StartingClass.tiledirt;
        } else if (type == 8) {
            tileImage = StartingClass.tilegrassTop;
        } else if (type == 4) {
            tileImage = StartingClass.tilegrassLeft;
        } else if (type == 6) {
            tileImage = StartingClass.tilegrassRight;
        } else if (type == 2) {
            tileImage = StartingClass.tilegrassBot;
        }
    }

    public void update() {
        if (type == 1) {
            if (bg.getBgSpeedX() == 0) {
                tileSpeedX = -1;
            } else {
                tileSpeedX = -2;
            }
        } else {
            tileSpeedX = bg.getBgSpeedX() * 5;
        }
        tileX += tileSpeedX;
    }

    public int getTileX() {
        return tileX;
    }

    public int getTileY() {
        return tileY;
    }

    public Image getTileImage() {
        return tileImage;
    }

    public void setTileX(int tileX) {
        this.tileX = tileX;
    }

    public void setTileY(int tileY) {
        this.tileY = tileY;
    }

    public void setTileImage(Image tileImage) {
        this.tileImage = tileImage;
    }

}