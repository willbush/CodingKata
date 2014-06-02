package com.kilobolt.GameWorld;

import com.badlogic.gdx.math.Intersector;
import com.badlogic.gdx.math.Rectangle;
import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameObjects.ScrollHandler;
import com.kilobolt.ZBHelpers.AssetLoader;

public class GameWorld {
    private final Bird bird;
    private final ScrollHandler scroller;
    private final Rectangle ground;
    private final int myMidPointY;
    private final int birdStartingY;
    private int score = 0;
    private GameState currentState;

    public enum GameState {
        READY, RUNNING, GAMEOVER, HIGHSCORE
    }

    public GameWorld(final int midPointY) {
        currentState = GameState.RUNNING;
        myMidPointY = midPointY;
        birdStartingY = midPointY - 5;
        final int birdStartingX = 33;
        final int birdWidth = 17;
        final int birdHeight = 12;
        bird = new Bird(birdStartingX, birdStartingY,
                birdWidth, birdHeight);
        final int distanceBelowMidPoint = 66 + midPointY;
        scroller = new ScrollHandler(this, distanceBelowMidPoint);
        ground = new Rectangle(0, midPointY + 66, 136, 11);
    }

    public final void update(final float delta) {
        if (currentState == GameState.READY) {
            updateReady(delta);
        } else if (currentState == GameState.RUNNING) {
            updateRunning(delta);
        }
    }

    public final void updateRunning(final float delta) {
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

    private void updateReady(final float delta) {
        // do nothing for now.
    }

    public final void start() {
        currentState = GameState.RUNNING;
    }

    public final void restart() {
        currentState = GameState.READY;
        score = 0;
        bird.onRestart(birdStartingY);
        scroller.onRestart();
        currentState = GameState.READY;
    }

    public final boolean isReady() {
        return currentState == GameState.READY;
    }

    public final boolean isRunning() {
        return currentState == GameState.RUNNING;
    }

    public final boolean isGameOver() {
        return currentState == GameState.GAMEOVER;
    }

    public final boolean isHighScore() {
        return currentState == GameState.HIGHSCORE;
    }

    public final Bird getBird() {
        return bird;
    }

    public final ScrollHandler getScroller() {
        return scroller;
    }

    public final int getScore() {
        return score;
    }

    public final void addScore(final int increment) {
        score += increment;
    }
}
