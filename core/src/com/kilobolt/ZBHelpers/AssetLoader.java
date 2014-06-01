package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.audio.Sound;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.Texture.TextureFilter;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.TextureRegion;

public final class AssetLoader {
    private static Texture texture;
    private static TextureRegion bg, grass;
    private static Animation birdAnimation;
    private static TextureRegion bird, birdDown, birdUp;
    private static TextureRegion skull, skullDown, skullUp, bar;
    private static Sound dead, flap, coin;
    private static BitmapFont font, shadow;
    private static final int GAME_WIDTH = 136;

    private AssetLoader() {
        throw new AssertionError("Instantiating utility class...");
    }

    public static void load() {
        loadTextures();
        loadBackground();
        loadGrass();
        loadBird();
        loadBirdAnimiation();
        loadSkull();
        loadBar();
        loadAudio();
        loadFonts();
    }

    private static void loadTextures() {
        texture = new Texture(Gdx.files.internal("data/texture.png"));
        texture.setFilter(TextureFilter.Nearest, TextureFilter.Nearest);
    }

    private static void loadBackground() {
        bg = new TextureRegion(texture, 0, 0, GAME_WIDTH, 43);
        // flipping because we are using "Y down" coordinate system.
        bg.flip(false, true);
    }

    private static void loadGrass() {
        grass = new TextureRegion(texture, 0, 43, 143, 11);
        grass.flip(false, true);
    }

    private static void loadBird() {
        birdDown = new TextureRegion(texture, GAME_WIDTH, 0, 17, 12);
        birdDown.flip(false, true);
        bird = new TextureRegion(texture, 153, 0, 17, 12);
        bird.flip(false, true);
        birdUp = new TextureRegion(texture, 170, 0, 17, 12);
        birdUp.flip(false, true);
    }

    private static void loadBirdAnimiation() {
        final TextureRegion[] birds = { birdDown, bird, birdUp };
        birdAnimation = new Animation(0.06f, birds);
        birdAnimation.setPlayMode(Animation.PlayMode.LOOP_PINGPONG);
    }

    private static void loadSkull() {
        skullUp = new TextureRegion(texture, 192, 0, 24, 14);
        // Create by flipping existing skullUp
        skullDown = new TextureRegion(skullUp);
        skullDown.flip(false, true);
    }

    private static void loadBar() {
        bar = new TextureRegion(texture, GAME_WIDTH, 16, 22, 3);
        bar.flip(false, true);
    }

    private static void loadAudio() {
        dead = Gdx.audio.newSound(Gdx.files.internal("data/dead.wav"));
        flap = Gdx.audio.newSound(Gdx.files.internal("data/flap.wav"));
        coin = Gdx.audio.newSound(Gdx.files.internal("data/coin.wav"));
    }

    private static void loadFonts() {
        font = new BitmapFont(Gdx.files.internal("data/text.fnt"));
        font.setScale(.25f, -.25f);
        shadow = new BitmapFont(Gdx.files.internal("data/shadow.fnt"));
        shadow.setScale(.25f, -.25f);
    }

    public static void dispose() {
        texture.dispose();
        dead.dispose();
        flap.dispose();
        coin.dispose();
        font.dispose();
        shadow.dispose();
    }

    public static TextureRegion getBg() {
        return bg;
    }

    public static TextureRegion getGrass() {
        return grass;
    }

    public static Animation getBirdAnimation() {
        return birdAnimation;
    }

    public static TextureRegion getBird() {
        return bird;
    }

    public static TextureRegion getBirdDown() {
        return birdDown;
    }

    public static TextureRegion getBirdUp() {
        return birdUp;
    }

    public static TextureRegion getSkull() {
        return skull;
    }

    public static TextureRegion getSkullDown() {
        return skullDown;
    }

    public static TextureRegion getSkullUp() {
        return skullUp;
    }

    public static TextureRegion getBar() {
        return bar;
    }

    public static Sound getDead() {
        return dead;
    }

    public static Sound getFlap() {
        return flap;
    }

    public static Sound getCoin() {
        return coin;
    }

    public static BitmapFont getFont() {
        return font;
    }

    public static BitmapFont getShadow() {
        return shadow;
    }
}
