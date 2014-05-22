package kiloboltgame;

import java.awt.Rectangle;
import java.util.ArrayList;

public class Robot {

    public static Rectangle headBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle rightTorsoBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle leftTorsoBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle feetBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle rightHandBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle leftHandBox = new Rectangle(0, 0, 0, 0);
    public static Rectangle checkCollisionBox = new Rectangle(0, 0, 0, 0);

    private static final int JUMP_SPEED = -15;
    private static final int WALK_SPEED = 5;
    private static final int STARTING_X_POS = 100;
    private static final int STARTING_Y_POS = 377;
    private static final int START_SCROLLING_POS = 390;

    private static Background bg1 = StartingClass.getBg1();
    private static Background bg2 = StartingClass.getBg2();

    private int robotPosX = STARTING_X_POS;
    private int robotPosY = STARTING_Y_POS;

    private int robotSpeedX = 0;
    private int robotSpeedY = 0;

    private boolean jumped = false;
    private boolean movingLeft = false;
    private boolean movingRight = false;
    private boolean ducked = false;
    private boolean readyToFire = true;

    private ArrayList<Projectile> projectiles = new ArrayList<Projectile>();

    public void update() {
        handleWalking();
        handleBackgroundScrolling();
        handleJumping();
        preventRobotMovingOutOfFrame();
        robotCollisionBoxes();
        robotCheckColisionBox();
    }

    private void handleWalking() {
        if (robotSpeedX < 0) {
            robotPosX += robotSpeedX;
        }
        if (robotPosX <= START_SCROLLING_POS && robotSpeedX > 0) {
            robotPosX += robotSpeedX;
        }
    }

    private void handleBackgroundScrolling() {
        if (robotSpeedX == 0 || robotSpeedX < 0) {
            bg1.setbGspeedX(0);
            bg2.setbGspeedX(0);
        }
        if (robotSpeedX > 0 && robotPosX > START_SCROLLING_POS) {
            bg1.setbGspeedX(-WALK_SPEED / 5);
            bg2.setbGspeedX(-WALK_SPEED / 5);
        }
    }

    private void handleJumping() {
        robotPosY += robotSpeedY;
        robotSpeedY += 1;
        if (robotSpeedY > 3)
            jumped = true;
    }

    private void preventRobotMovingOutOfFrame() {
        if (robotPosX + robotSpeedX <= 60) {
            robotPosX = 61;
        }
    }

    private void robotCollisionBoxes() {
        headBox.setRect(robotPosX - 14, robotPosY - 65, 28, 28);
        leftTorsoBox.setRect(robotPosX - 26, robotPosY - 32, 26, 64);
        rightTorsoBox.setRect(robotPosX, robotPosY - 32, 26, 64);
        leftHandBox.setRect(robotPosX - 60, robotPosY - 31, 26, 20);
        rightHandBox.setRect(robotPosX + 34, robotPosY - 31, 26, 20);
        feetBox.setRect(robotPosX - 29, robotPosY + 34, 58, 30);
    }

    private void robotCheckColisionBox() {
        checkCollisionBox.setRect(robotPosX - 90, robotPosY - 90, 180, 180);
    }

    public void moveRight() {
        if (!ducked)
            robotSpeedX = WALK_SPEED;
    }

    public void moveLeft() {
        if (!ducked)
            robotSpeedX = -WALK_SPEED;
    }

    public void stopRight() {
        movingRight = false;
        updateHorizontalMovement();
    }

    public void stopLeft() {
        movingLeft = false;
        updateHorizontalMovement();
    }

    private void updateHorizontalMovement() {
        if (movingRight == false && movingLeft == false)
            robotSpeedX = 0;
        if (movingRight == false && isMovingLeft())
            moveLeft();
        if (isMovingRight() && isMovingLeft())
            moveRight();
    }

    public void jump() {
        if (jumped == false) {
            robotSpeedY = JUMP_SPEED;
            jumped = true;
        }
    }

    public void shoot() {
        if (isReadyToFire()) {
            Projectile p = new Projectile(robotPosX + 50, robotPosY - 25);
            projectiles.add(p);
        }
    }

    public int getRobotPosX() {
        return robotPosX;
    }

    public int getRobotPosY() {
        return robotPosY;
    }

    public int getRobotSpeedX() {
        return robotSpeedX;
    }

    public int getRobotSpeedY() {
        return robotSpeedY;
    }

    public boolean isJumped() {
        return jumped;
    }

    public boolean isMovingLeft() {
        return movingLeft;
    }

    public boolean isMovingRight() {
        return movingRight;
    }

    public boolean isDucked() {
        return ducked;
    }

    public boolean isReadyToFire() {
        return readyToFire;
    }

    public ArrayList<Projectile> getProjectiles() {
        return projectiles;
    }

    public void setRobotPosX(int robotPosX) {
        this.robotPosX = robotPosX;
    }

    public void setRobotPosY(int robotPosY) {
        this.robotPosY = robotPosY;
    }

    public void setRobotSpeedX(int robotSpeedX) {
        this.robotSpeedX = robotSpeedX;
    }

    public void setRobotSpeedY(int robotSpeedY) {
        this.robotSpeedY = robotSpeedY;
    }

    public void setJumped(boolean jumped) {
        this.jumped = jumped;
    }

    public void setMovingLeft(boolean movingLeft) {
        this.movingLeft = movingLeft;
    }

    public void setMovingRight(boolean movingRight) {
        this.movingRight = movingRight;
    }

    public void setDucked(boolean ducked) {
        this.ducked = ducked;
    }

    public void setReadyToFire(boolean readyToFire) {
        this.readyToFire = readyToFire;
    }
}
