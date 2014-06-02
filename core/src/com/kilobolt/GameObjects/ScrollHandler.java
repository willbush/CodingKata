/*
 * ScrollHandler will use the constants below to determine
 * how fast we need to scroll and also determine
 * the size of the gap between each pair of pipes.
 */
package com.kilobolt.GameObjects;

import com.kilobolt.GameWorld.GameWorld;
import com.kilobolt.ZBHelpers.AssetLoader;

public class ScrollHandler {
    private final GameWorld myGameWorld;
    private final Grass frontGrass, backGrass;
    private final Pipe pipe1, pipe2, pipe3;
    private static final int SCROLL_SPEED = -59;
    private static final int PIPE_GAP = 49;
    private static final int PIPE_WIDTH = 22;
    private static final int PIPE_HEIGHT = 60;
    private static final int PIPE1_STARTING_POS = 210;

    public ScrollHandler(final GameWorld gameWorld, final float yPos) {
        myGameWorld = gameWorld;
        frontGrass = new Grass(0, yPos, 143, 11, SCROLL_SPEED);
        backGrass = new Grass(frontGrass.getTailX(), yPos, 143, 11,
                SCROLL_SPEED);
        pipe1 = new Pipe(PIPE1_STARTING_POS, 0, PIPE_WIDTH,
                PIPE_HEIGHT, SCROLL_SPEED, yPos);
        pipe2 = new Pipe(pipe1.getTailX() + PIPE_GAP, 0, PIPE_WIDTH,
                PIPE_HEIGHT, SCROLL_SPEED, yPos);
        pipe3 = new Pipe(pipe2.getTailX() + PIPE_GAP, 0, PIPE_WIDTH,
                PIPE_HEIGHT, SCROLL_SPEED, yPos);
    }

    public final void update(final float delta) {
        updateObjects(delta);
        resetScrollablePipes();
        resetScrollableGrass();
    }

    public final void handleScore(final Bird bird) {
        if (hasScored(pipe1, bird)) {
            updateScore(pipe1);
        } else if (hasScored(pipe2, bird)) {
            updateScore(pipe2);
        } else if (hasScored(pipe3, bird)) {
            updateScore(pipe3);
        }
    }

    private boolean hasScored(final Pipe pipe, final Bird bird) {
        return !pipe.isScored()
                && pipe.getX() + (pipe.getWidth() / 2) < bird.getX()
                        + bird.getWidth();
    }

    private void updateScore(final Pipe pipe) {
        addScore(1);
        pipe.setScored(true);
        AssetLoader.getCoin().play();
    }

    private void updateObjects(final float delta) {
        frontGrass.update(delta);
        backGrass.update(delta);
        pipe1.update(delta);
        pipe2.update(delta);
        pipe3.update(delta);
    }

    private void resetScrollablePipes() {
        if (pipe1.objectIsScrollableRight()) {
            pipe1.reset(pipe3.getTailX() + PIPE_GAP);
        } else if (pipe2.objectIsScrollableRight()) {
            pipe2.reset(pipe1.getTailX() + PIPE_GAP);
        } else if (pipe3.objectIsScrollableRight()) {
            pipe3.reset(pipe2.getTailX() + PIPE_GAP);
        }
    }

    private void resetScrollableGrass() {
        if (frontGrass.objectIsScrollableRight()) {
            frontGrass.reset(backGrass.getTailX());
        } else if (backGrass.objectIsScrollableRight()) {
            backGrass.reset(frontGrass.getTailX());
        }
    }

    public final void stop() {
        frontGrass.stop();
        backGrass.stop();
        pipe1.stop();
        pipe2.stop();
        pipe3.stop();
    }

    public final boolean hasCollided(final Bird bird) {
        return (pipe1.hasCollided(bird) || pipe2.hasCollided(bird) || pipe3
                .hasCollided(bird));
    }

    public final Grass getFrontGrass() {
        return frontGrass;
    }

    public final Grass getBackGrass() {
        return backGrass;
    }

    public final Pipe getPipe1() {
        return pipe1;
    }

    public final Pipe getPipe2() {
        return pipe2;
    }

    public final Pipe getPipe3() {
        return pipe3;
    }

    private void addScore(final int increment) {
        myGameWorld.addScore(increment);
    }

    public final void onRestart() {
        frontGrass.onRestart(0, SCROLL_SPEED);
        backGrass.onRestart(frontGrass.getTailX(), SCROLL_SPEED);
        pipe1.onRestart(PIPE1_STARTING_POS, SCROLL_SPEED);
        pipe2.onRestart(pipe1.getTailX() + PIPE_GAP, SCROLL_SPEED);
        pipe3.onRestart(pipe2.getTailX() + PIPE_GAP, SCROLL_SPEED);
    }
}
