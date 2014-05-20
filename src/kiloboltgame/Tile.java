package kiloboltgame;

import java.awt.Image;
import java.awt.Rectangle;

public class Tile {

    public Image tileImage;

    private int tilePosX, tilePosY, tileSpeedX, tileType;
    private Robot robot = StartingClass.getRobot();
    private Background bg = StartingClass.getBg1();
    private Rectangle tileCollisionBox;

    public Tile(int x, int y, int typeInt) {
        tilePosX = x * 40;
        tilePosY = y * 40;

        tileType = typeInt;

        tileCollisionBox = new Rectangle();

        handleTileType();
    }

    private void handleTileType() {
        if (tileType == 5) {
            tileImage = StartingClass.tiledirt;
        } else if (tileType == 8) {
            tileImage = StartingClass.tilegrassTop;
        } else if (tileType == 4) {
            tileImage = StartingClass.tilegrassLeft;
        } else if (tileType == 6) {
            tileImage = StartingClass.tilegrassRight;
        } else if (tileType == 2) {
            tileImage = StartingClass.tilegrassBot;
        } else {
            tileType = 0;
        }
    }

    public void update() {
        tileSpeedX = bg.getBgSpeedX() * 5;
        tilePosX += tileSpeedX;
        handleTileCollision();
    }

    private void handleTileCollision() {
        tileCollisionBox.setBounds(tilePosX, tilePosY, 40, 40);

        if (tileCollisionBox.intersects(Robot.checkCollisionBox)
                && tileType != 0) {
            checkVerticleCollision(Robot.headBox, Robot.feetBox);
            checkHorizontalCollision(Robot.leftTorsoBox, Robot.rightTorsoBox);
        }
    }

    public void checkVerticleCollision(Rectangle rTop, Rectangle rBot) {
        if (rBot.intersects(tileCollisionBox) && tileType == 8) {
            robot.setJumped(false);
            robot.setRobotSpeedY(0);
            robot.setRobotPosY(tilePosY - 63);
        }
    }

    private void checkHorizontalCollision(Rectangle rLeft, Rectangle rRight) {
        if (tileType != 5 && tileType != 2 && tileType != 0) {
            if (rLeft.intersects(tileCollisionBox)) {
                robot.setRobotPosX(tilePosX + 70);
                robot.setRobotSpeedX(0);
            }
            if (rRight.intersects(tileCollisionBox)) {
                robot.setRobotPosX(tilePosX - 30);
                robot.setRobotSpeedX(0);
            }
        }
    }

    public int getTileX() {
        return tilePosX;
    }

    public int getTileY() {
        return tilePosY;
    }

    public Image getTileImage() {
        return tileImage;
    }

    public void setTileX(int tileX) {
        this.tilePosX = tileX;
    }

    public void setTileY(int tileY) {
        this.tilePosY = tileY;
    }

    public void setTileImage(Image tileImage) {
        this.tileImage = tileImage;
    }
}