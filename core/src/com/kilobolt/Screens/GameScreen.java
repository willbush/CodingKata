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
    private final float gameHeight = calculateGameHeight();
    private static final float GAME_WIDTH = 136;

    public GameScreen() {
        final int midPointY = calculateMidPointY();
        world = new GameWorld(midPointY);
        renderer = new GameRenderer(world, (int) gameHeight, midPointY);
        Gdx.input.setInputProcessor(new InputHandler(world.getBird()));
    }

    private int calculateMidPointY() {
        final int midPointY = (int) (gameHeight / 2);
        return midPointY;
    }

    private float calculateGameHeight() {
        final float screenWidth = Gdx.graphics.getWidth();
        final float screenHeight = Gdx.graphics.getHeight();
        final float gameHeight = screenHeight / (screenWidth / GAME_WIDTH);
        return gameHeight;
    }

    @Override
    public void render(float delta) {
        runTime += delta;
        world.update(delta);
        renderer.render(runTime);
    }

    @Override
    public void resize(int width, int height) {
        System.out.println("GameScreen - resize called");
    }

    @Override
    public void show() {
        System.out.println("GameScreen - show called");
    }

    @Override
    public void hide() {
        System.out.println("GameScreen - hide called");
    }

    @Override
    public void pause() {
        System.out.println("GameScreen - pause called");
    }

    @Override
    public void resume() {
        System.out.println("GameScreen - resume called");
    }

    @Override
    public void dispose() {
    }
}
