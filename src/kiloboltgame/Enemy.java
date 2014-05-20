package kiloboltgame;

import java.awt.Rectangle;

public class Enemy {

    public Rectangle enemyCollisionBox = new Rectangle(0, 0, 0, 0);

    private int maxHealth, currentHealth, damageOutput, enemySpeedX,
            enemyPositionX, enemyPositionY;
    private Background bg = StartingClass.getBg1();

    public void update() {
        enemyPositionX += enemySpeedX;
        enemySpeedX = bg.getBgSpeedX() * 5;
        enemyCollisionBox.setBounds(enemyPositionX - 25, enemyPositionY - 25, 50, 60);

        if (enemyCollisionBox.intersects(Robot.checkCollisionBox)) {
            checkCollision();
        }
    }

    private void checkCollision() {
        if (enemyCollisionBox.intersects(Robot.headBox) || enemyCollisionBox.intersects(Robot.rightTorsoBox)
                || enemyCollisionBox.intersects(Robot.leftTorsoBox)) {
            System.out.println("enemy collision");
        }
    }

    public void die() {
    }

    public void attack() {
    }

    public int getMaxHealth() {
        return maxHealth;
    }

    public int getCurrentHealth() {
        return currentHealth;
    }

    public int getDamageOutput() {
        return damageOutput;
    }

    public int getEnemySpeedX() {
        return enemySpeedX;
    }

    public int getEnemyPositionX() {
        return enemyPositionX;
    }

    public int getEnemyPositionY() {
        return enemyPositionY;
    }

    public Background getBg() {
        return bg;
    }

    public void setMaxHealth(int maxHealth) {
        this.maxHealth = maxHealth;
    }

    public void setCurrentHealth(int currentHealth) {
        this.currentHealth = currentHealth;
    }

    public void setDamageOutput(int damageOutput) {
        this.damageOutput = damageOutput;
    }

    public void setEnemySpeedX(int enemySpeedX) {
        this.enemySpeedX = enemySpeedX;
    }

    public void setEnemyPositionX(int enemyPositionX) {
        this.enemyPositionX = enemyPositionX;
    }

    public void setEnemyPositionY(int enemyPositionY) {
        this.enemyPositionY = enemyPositionY;
    }

    public void setBg(Background bg) {
        this.bg = bg;
    }
}
