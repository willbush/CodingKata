package kiloboltgame;

import java.awt.Rectangle;

public class Projectile {

    private int xPosition, yPosition, projectileSpeedX;
    private boolean visible;
    private Rectangle projectileCollisionBox;

    public Projectile(int startPosX, int startPosY) {
        xPosition = startPosX;
        yPosition = startPosY;
        projectileSpeedX = 7;
        visible = true;
        projectileCollisionBox = new Rectangle(0, 0, 0, 0);
    }

    public void update() {
        xPosition += projectileSpeedX;
        projectileCollisionBox.setBounds(xPosition, yPosition, 10, 5);
        if (isOnScreen())
            collisionCheck();
        if (!isOnScreen()) {
            visible = false;
            projectileCollisionBox = null;
        }
    }

    private void collisionCheck() {
        if (projectileCollisionBox
                .intersects(StartingClass.hb.enemyCollisionBox)
                || projectileCollisionBox
                        .intersects(StartingClass.hb2.enemyCollisionBox)) {
            visible = false;
            StartingClass.score += 1;
        }
    }

    private boolean isOnScreen() {
        return xPosition >= 0 && xPosition <= 800;
    }

    public int getxPosition() {
        return xPosition;
    }

    public int getyPosition() {
        return yPosition;
    }

    public boolean isVisible() {
        return visible;
    }
}
