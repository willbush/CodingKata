package com.kilobolt.Screens;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.kilobolt.GameWorld.GameRenderer;
import com.kilobolt.GameWorld.GameWorld;
import com.kilobolt.ZBHelpers.InputHandler;

public final class GameScreen implements Screen {

    private final GameWorld world;
    private final GameRenderer renderer;
    private float runTime;

    public GameScreen() {
        final float screenWidth = Gdx.graphics.getWidth();
        final float screenHeight = Gdx.graphics.getHeight();
        final float gameWidth = GameRenderer.GAME_WIDTH;
        final float gameHeight = screenHeight / (screenWidth / gameWidth);
        final int midPointY = (int) (gameHeight / 2);

        world = new GameWorld(midPointY);
        final float scaleFactorX = screenWidth / gameWidth;
        final float scaleFactorY = screenHeight / gameHeight;
        Gdx.input.setInputProcessor(new InputHandler(world, scaleFactorX,
                scaleFactorY));
        renderer = new GameRenderer(world, (int) gameHeight, midPointY);
    }

    @Override
    public void render(final float delta) {
        runTime += delta;
        world.update(delta);
        renderer.render(delta, runTime);
    }

    @Override
    public void resize(final int width, final int height) {
    }

    @Override
    public void show() {
    }

    @Override
    public void hide() {
    }

    @Override
    public void pause() {
    }

    @Override
    public void resume() {
    }

    @Override
    public void dispose() {
    }

}
