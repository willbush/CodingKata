package kiloboltgame;

public class Heliboy extends Enemy {

    public int health = 5;

    public Heliboy(int enemyPositionX, int enemyPositionY) {
        setEnemyPosX(enemyPositionX);
        setEnemyPosY(enemyPositionY);
    }
}
