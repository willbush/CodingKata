package kiloboltgame;

import java.awt.Rectangle;

public class Enemy {

    public Rectangle enemyCollisionBox = new Rectangle(0, 0, 0, 0);

    private int enemySpeedX, enemyPosX, enemyPosY, movementSpeed;
    private Background bg = StartingClass.getBg1();
    private Robot robot = StartingClass.getRobot();

    public void update() {
        follow();
        enemyPosX += enemySpeedX;
        enemySpeedX = bg.getbGspeedX() * 5 + movementSpeed;
        enemyCollisionBox.setBounds(enemyPosX - 25, enemyPosY - 25, 50, 60);

        if (enemyCollisionBox.intersects(Robot.checkCollisionBox)) {
            checkCollision();
        }
    }

    private void follow() {
        if (enemyPosX < -95 || enemyPosX > 810)
            movementSpeed = 0;
        else if (Math.abs(robot.getRobotPosX() - enemyPosX) < 5)
            movementSpeed = 0;
        else {
            if (robot.getRobotPosX() >= enemyPosX)
                movementSpeed = 1;
            else
                movementSpeed = -1;
        }
    }

    private void checkCollision() {
        if (enemyCollisionBox.intersects(Robot.headBox)
                || enemyCollisionBox.intersects(Robot.rightTorsoBox)
                || enemyCollisionBox.intersects(Robot.leftTorsoBox)) {
            System.out.println("enemy collision");
        }
    }

    public int getEnemyPosX() {
        return enemyPosX;
    }

    public int getEnemyPosY() {
        return enemyPosY;
    }

    public void setEnemyPosX(int enemyPosX) {
        this.enemyPosX = enemyPosX;
    }

    public void setEnemyPosY(int enemyPosY) {
        this.enemyPosY = enemyPosY;
    }
}
