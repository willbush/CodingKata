package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.InputProcessor;
import com.kilobolt.GameObjects.Bird;

public class InputHandler implements InputProcessor {
    private final Bird myBird;

    public InputHandler(final Bird bird) {
        myBird = bird;
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
        myBird.onClick();
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
