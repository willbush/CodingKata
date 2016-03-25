package com.kilobolt.ZBHelpers;

import java.util.ArrayList;
import java.util.List;

import com.badlogic.gdx.Input.Keys;
import com.badlogic.gdx.InputProcessor;
import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameWorld.GameRenderer;
import com.kilobolt.GameWorld.GameWorld;
import com.kilobolt.UI.SimpleButton;

public final class InputHandler implements InputProcessor {
    private Bird myBird;
    private GameWorld world;

    private List<SimpleButton> menuButtons;

    private SimpleButton playButton;

    private float scaleFactorX;
    private float scaleFactorY;

    public InputHandler(GameWorld gw, float scaleFactorX,
            float scaleFactorY) {
        world = gw;
        myBird = gw.getBird();

        int midPointY = gw.getMidPointY();

        this.scaleFactorX = scaleFactorX;
        this.scaleFactorY = scaleFactorY;

        menuButtons = new ArrayList<SimpleButton>();
        playButton = new SimpleButton(GameRenderer.GAME_WIDTH / 2
                - (AssetLoader.getPlayButtonUp().getRegionWidth() / 2),
                midPointY + 50, 29, 16, AssetLoader.getPlayButtonUp(),
                AssetLoader.getPlayButtonDown());
        menuButtons.add(playButton);
    }

    @Override
    public boolean touchDown(int screenX, int screenY, int pointer,
            int button) {
        screenX = scaleX(screenX);
        screenY = scaleY(screenY);
        System.out.println(screenX + " " + screenY);
        if (world.isMenu()) {
            playButton.isTouchDown(screenX, screenY);
        } else if (world.isReady()) {
            world.start();
        }

        myBird.onClick();

        if (world.isGameOver() || world.isHighScore()) {
            world.restart();
        }

        return true;
    }

    @Override
    public boolean touchUp(int screenX, int screenY, int pointer, int button) {
        screenX = scaleX(screenX);
        screenY = scaleY(screenY);

        if (world.isMenu()) {
            if (playButton.isTouchUp(screenX, screenY)) {
                world.ready();
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean keyDown(int keycode) {

        // Can now use Space Bar to play the game
        if (keycode == Keys.SPACE) {

            if (world.isMenu()) {
                world.ready();
            } else if (world.isReady()) {
                world.start();
            }

            myBird.onClick();

            if (world.isGameOver() || world.isHighScore()) {
                world.restart();
            }

        }
        return false;
    }

    @Override
    public boolean keyUp(int keycode) {
        return false;
    }

    @Override
    public boolean keyTyped(char character) {
        return false;
    }

    @Override
    public boolean touchDragged(int screenX, int screenY, int pointer) {
        return false;
    }

    @Override
    public boolean mouseMoved(int screenX, int screenY) {
        return false;
    }

    @Override
    public boolean scrolled(int amount) {
        return false;
    }

    private int scaleX(int screenX) {
        return (int) (screenX / scaleFactorX);
    }

    private int scaleY(int screenY) {
        return (int) (screenY / scaleFactorY);
    }

    public List<SimpleButton> getMenuButtons() {
        return menuButtons;
    }
}
