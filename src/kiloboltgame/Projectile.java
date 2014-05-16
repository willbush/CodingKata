package kiloboltgame;

public class Projectile {

    private int xPosition, yPosition, projectileSpeedX, projectileSpeedY;
    private boolean visible;

    public Projectile(int startPosX, int startPosY) {
        xPosition = startPosX;
        yPosition = startPosY;
        startPosX = 7;
        visible = true;
    }

    public void update() {
        xPosition += projectileSpeedX;
        if (isOnScreen())
            visible = false;
    }

    private boolean isOnScreen() {
        return xPosition > 800;
    }

    public int getxPosition() {
        return xPosition;
    }

    public int getyPosition() {
        return yPosition;
    }

    public int getProjectileSpeedX() {
        return projectileSpeedX;
    }

    public int getProjectileSpeedY() {
        return projectileSpeedY;
    }

    public boolean isVisible() {
        return visible;
    }

    public void setxPosition(int xPosition) {
        this.xPosition = xPosition;
    }

    public void setyPosition(int yPosition) {
        this.yPosition = yPosition;
    }

    public void setProjectileSpeedX(int projectileSpeedX) {
        this.projectileSpeedX = projectileSpeedX;
    }

    public void setProjectileSpeedY(int projectileSpeedY) {
        this.projectileSpeedY = projectileSpeedY;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }
}
