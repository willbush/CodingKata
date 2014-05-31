package com.kilobolt.Screens;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.kilobolt.GameWorld.GameRenderer;
import com.kilobolt.GameWorld.GameWorld;
import com.kilobolt.ZBHelpers.InputHandler;

public class GameScreen implements Screen {
    private final GameWorld world;
    private final GameRenderer renderer;
    private float runTime;
    private final float myGameHeight = calculateGameHeight();
    private static final float GAME_WIDTH = 136;

    public GameScreen() {
        final int midPointY = calculateMidPointY();
        world = new GameWorld(midPointY);
        renderer = new GameRenderer(world, (int) myGameHeight, midPointY);
        Gdx.input.setInputProcessor(new InputHandler(world.getBird()));
    }

    private int calculateMidPointY() {
        final int midPointY = (int) (myGameHeight / 2);
        return midPointY;
    }

    private float calculateGameHeight() {
        final float screenWidth = Gdx.graphics.getWidth();
        final float screenHeight = Gdx.graphics.getHeight();
        final float gameHeight = screenHeight / (screenWidth / GAME_WIDTH);
        return gameHeight;
    }

    @Override
    public final void render(final float delta) {
        runTime += delta;
        world.update(delta);
        renderer.render(runTime);
    }

    @Override
    public final void resize(final int width, final int height) {
        System.out.println("GameScreen - resize called");
    }

    @Override
    public final void show() {
        System.out.println("GameScreen - show called");
    }

    @Override
    public final void hide() {
        System.out.println("GameScreen - hide called");
    }

    @Override
    public final void pause() {
        System.out.println("GameScreen - pause called");
    }

    @Override
    public final void resume() {
        System.out.println("GameScreen - resume called");
    }

    @Override
    public void dispose() {
    }
}
