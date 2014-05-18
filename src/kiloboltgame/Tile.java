package kiloboltgame;

import java.awt.Image;
import java.awt.Rectangle;

import org.w3c.dom.css.Rect;

public class Tile {

    public Image tileImage;

    private int tileX, tileY, tileSpeedX, type;
    private Robot robot = StartingClass.getRobot();
    private Background bg = StartingClass.getBg1();
    private Rectangle tileCollisionBox;

    public Tile(int x, int y, int typeInt) {
        tileX = x * 40;
        tileY = y * 40;

        type = typeInt;

        tileCollisionBox = new Rectangle();

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
        } else {
            type = 0;
        }
    }

    public void update() {
        tileSpeedX = bg.getBgSpeedX() * 5;
        tileX += tileSpeedX;
        handleTileCollision();
    }

    private void handleTileCollision() {
        tileCollisionBox.setBounds(tileX, tileY, 40, 40);

        if (type != 0) {
            checkVerticleCollision(Robot.rect, Robot.rect2);
        }
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

    public void checkVerticleCollision(Rectangle rtop, Rectangle rbot) {
        if (rtop.intersects(tileCollisionBox)) {
            System.out.println("upper collision");
        }
        if (rbot.intersects(tileCollisionBox)) {
            System.out.println("lower collision");
        }

    }

}