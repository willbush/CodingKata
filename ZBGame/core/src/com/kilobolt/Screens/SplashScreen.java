package com.kilobolt.Screens;

import aurelienribon.tweenengine.BaseTween;
import aurelienribon.tweenengine.Tween;
import aurelienribon.tweenengine.TweenCallback;
import aurelienribon.tweenengine.TweenEquations;
import aurelienribon.tweenengine.TweenManager;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.g2d.Sprite;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.kilobolt.TweenAccessors.SpriteAccessor;
import com.kilobolt.ZBHelpers.AssetLoader;
import com.kilobolt.ZombieBird.ZBGame;

public final class SplashScreen implements Screen {

    private TweenManager manager;
    private SpriteBatch batcher;
    private Sprite logoSprite;
    private final ZBGame game;

    public SplashScreen(ZBGame game) {
        this.game = game;
    }

    @Override
    public void show() {
        batcher = new SpriteBatch();
        setupLogoSprite();
        setupTween();
    }

    private void setupLogoSprite() {
        logoSprite = new Sprite(AssetLoader.getLogo());
        logoSprite.setColor(1, 1, 1, 0);
        
        final float width = Gdx.graphics.getWidth();
        final float height = Gdx.graphics.getHeight();
        final float desiredWidth = width * .7f;
        final float scale = desiredWidth / logoSprite.getWidth();

        logoSprite.setSize(logoSprite.getWidth() * scale,
                logoSprite.getHeight() * scale);
        logoSprite.setPosition((width / 2) - (logoSprite.getWidth() / 2),
                (height / 2) - (logoSprite.getHeight() / 2));
    }

    private void setupTween() {
        Tween.registerAccessor(Sprite.class, new SpriteAccessor());
        manager = new TweenManager();

        final TweenCallback cb = new TweenCallback() {
            @Override
            public void onEvent(int type, BaseTween<?> source) {
                game.setScreen(new GameScreen());
            }
        };
        generateLogoWithAlphaYoyo(cb);
    }

    private void generateLogoWithAlphaYoyo(TweenCallback cb) {
        final float duration = .8f;
        final float delay = .4f;

        Tween.to(logoSprite, SpriteAccessor.ALPHA, duration).target(1)
                .ease(TweenEquations.easeInOutQuad).repeatYoyo(1, delay)
                .setCallback(cb).setCallbackTriggers(TweenCallback.COMPLETE)
                .start(manager);
    }

    @Override
    public void render(float delta) {
        manager.update(delta);
        clearScreenAndBuffer();
        batcher.begin();
        logoSprite.draw(batcher);
        batcher.end();
    }

    private void clearScreenAndBuffer() {
        Gdx.graphics.getGL20().glClearColor(1, 1, 1, 1);
        Gdx.graphics.getGL20().glClear(
                GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT);
    }

    @Override
    public void resize(int width, int height) {

    }

    @Override
    public void hide() {
        // TODO Auto-generated method stub

    }

    @Override
    public void pause() {
        // TODO Auto-generated method stub

    }

    @Override
    public void resume() {
        // TODO Auto-generated method stub

    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

}
