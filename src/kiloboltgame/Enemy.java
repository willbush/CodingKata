package kiloboltgame;

import java.awt.Rectangle;

public class Enemy {
    
    public Rectangle enemyCollisionBox = new Rectangle(0, 0, 0, 0);

    private int enemySpeedX, enemyPosX, enemyPosY;
    private Background bg = StartingClass.getBg1();

    public void update() {
        enemyPosX += enemySpeedX;
        enemySpeedX = bg.getbGspeedX() * 5;
        enemyCollisionBox.setBounds(enemyPosX - 25, enemyPosY - 25, 50, 60);

        if (enemyCollisionBox.intersects(Robot.checkCollisionBox)) {
            checkCollision();
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
