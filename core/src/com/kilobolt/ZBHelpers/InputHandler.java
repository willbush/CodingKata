package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.InputProcessor;
import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameWorld.GameWorld;

public class InputHandler implements InputProcessor {
    private final Bird myBird;
    private final GameWorld myWorld;

    public InputHandler(final GameWorld gameWorld) {
        myWorld = gameWorld;
        myBird = gameWorld.getBird();
    }

    @Override
    public final boolean keyDown(final int keycode) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean keyUp(final int keycode) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean keyTyped(final char character) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean touchDown(final int screenX, final int screenY,
            final int pointer, final int button) {
        if (myWorld.isReady()) {
            myWorld.start();
        }
        myBird.onClick();
        if (myWorld.isGameOver() || myWorld.isHighScore()) {
            myWorld.restart();
        }
        return true;
    }

    @Override
    public final boolean touchUp(final int screenX, final int screenY,
            final int pointer, final int button) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean touchDragged(final int screenX, final int screenY,
            final int pointer) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean mouseMoved(final int screenX, final int screenY) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public final boolean scrolled(final int amount) {
        // TODO Auto-generated method stub
        return false;
    }
}
