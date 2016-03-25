package com.kilobolt.GameWorld;

import com.badlogic.gdx.math.Intersector;
import com.badlogic.gdx.math.Rectangle;
import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameObjects.ScrollHandler;
import com.kilobolt.ZBHelpers.AssetLoader;

public final class GameWorld {
    private final Bird bird;
    private final ScrollHandler scroller;
    private final Rectangle ground;
    private final int midPointY;
    private final int birdStartingY;
    private int score = 0;
    private float runTime = 0;
    private GameState currentState;

    public enum GameState {
        MENU, READY, RUNNING, GAMEOVER, HIGHSCORE
    }

    public GameWorld(int midPointY) {
        this.midPointY = midPointY;
        final int birdStartingX = 33;
        final int birdWidth = 17;
        final int birdHeight = 12;
        final int birdPointShift = 5;
        final int pointShift = 66;
        final int midShiftedY = midPointY + pointShift;
        final int groundHeight = 11;

        currentState = GameState.MENU;
        birdStartingY = midPointY - birdPointShift;
        bird = new Bird(birdStartingX, birdStartingY, birdWidth, birdHeight);
        scroller = new ScrollHandler(this, midShiftedY);
        ground = new Rectangle(0, midShiftedY, GameRenderer.GAME_WIDTH,
                groundHeight);
    }

    public void update(float delta) {
        runTime += delta;

        switch (currentState) {
        case READY:
        case MENU:
            updateReady(delta);
            break;

        case RUNNING:
            updateRunning(delta);
            break;
        default:
            break;
        }
    }

    private void updateReady(float delta) {
        bird.updateReady(runTime);
        scroller.updateReady(delta);
    }

    public void updateRunning(float delta) {
        bird.update(delta);
        scroller.update(delta);
        scroller.handleScore(bird);
        checkObjectCollision();
        checkGroundCollision();
    }

    private void checkObjectCollision() {
        if (scroller.hasCollided(bird) && bird.isAlive()) {
            scroller.stop();
            bird.die();
            AssetLoader.getDead().play();
        }
    }

    private void checkGroundCollision() {
        if (Intersector.overlaps(bird.getCollisionCircle(), ground)) {
            scroller.stop();
            bird.die();
            bird.decelerate();
            currentState = GameState.GAMEOVER;
            checkScore();
        }
    }

    private void checkScore() {
        if (score > AssetLoader.getHighScore()) {
            AssetLoader.setHighScore(score);
            currentState = GameState.HIGHSCORE;
        }
    }

    public void ready() {
        currentState = GameState.READY;
    }

    public void start() {
        currentState = GameState.RUNNING;
    }

    public void restart() {
        currentState = GameState.READY;
        score = 0;
        bird.onRestart(birdStartingY);
        scroller.onRestart();
        currentState = GameState.READY;
    }

    public boolean isMenu() {
        return currentState == GameState.MENU;
    }

    public boolean isReady() {
        return currentState == GameState.READY;
    }

    public boolean isRunning() {
        return currentState == GameState.RUNNING;
    }

    public boolean isGameOver() {
        return currentState == GameState.GAMEOVER;
    }

    public boolean isHighScore() {
        return currentState == GameState.HIGHSCORE;
    }

    public Bird getBird() {
        return bird;
    }

    public ScrollHandler getScroller() {
        return scroller;
    }

    public int getScore() {
        return score;
    }

    public float getRunTime() {
        return runTime;
    }

    public int getMidPointY() {
        return midPointY;
    }

    public void addScore(int increment) {
        score += increment;
    }
}
