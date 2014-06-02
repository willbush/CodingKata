package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.audio.Sound;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.Texture.TextureFilter;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.TextureRegion;

public final class AssetLoader {
    private static Texture gameTextures, logoTexture;
    private static Animation birdAnimation;
    private static TextureRegion logo, zbLogo, bg, grass, bird, birdDown,
            birdUp, skull, skullDown,
            skullUp, bar, playButtonUp, playButtonDown;
    private static Sound dead, flap, coin;
    private static BitmapFont font, shadow;
    private static Preferences prefs;
    private static final int GAME_WIDTH = 136;

    private AssetLoader() {
        throw new AssertionError("Instantiating utility class...");
    }

    public static void load() {
        loadTextures();
        loadPlayButtons();
        loadZbLogo();
        loadBackground();
        loadGrass();
        loadBird();
        loadBirdAnimiation();
        loadSkull();
        loadBar();
        loadAudio();
        loadFonts();
        loadPreferencesFile();
    }

    private static void loadTextures() {
        final int width = 512;
        final int height = 114;

        logoTexture = new Texture(Gdx.files.internal("data/logo.png"));
        logoTexture.setFilter(TextureFilter.Linear, TextureFilter.Linear);
        logo = new TextureRegion(logoTexture, 0, 0, width, height);
        gameTextures = new Texture(Gdx.files.internal("data/texture.png"));
        gameTextures.setFilter(TextureFilter.Nearest, TextureFilter.Nearest);
    }

    private static void loadPlayButtons() {
        final int upY = 83;
        final int downY = 16;
        final int width = 29;
        final int height = 16;

        playButtonUp = new TextureRegion(gameTextures, 0, upY, width, height);
        playButtonDown = new TextureRegion(gameTextures, downY, upY,
                width, height);
        playButtonUp.flip(false, true);
        playButtonDown.flip(false, true);
    }

    private static void loadZbLogo() {
        final int y = 55;
        final int width = 135;
        final int height = 24;
        zbLogo = new TextureRegion(gameTextures, 0, y, width, height);
        zbLogo.flip(false, true);
    }

    private static void loadBackground() {
        final int height = 43;

        bg = new TextureRegion(gameTextures, 0, 0, GAME_WIDTH, height);
        // Flip because we are using "Y down" coordinate system.
        bg.flip(false, true);
    }

    private static void loadGrass() {
        final int y = 43;
        final int width = 143;
        final int height = 11;

        grass = new TextureRegion(gameTextures, 0, y, width,
                height);
        grass.flip(false, true);
    }

    private static void loadBird() {
        final int width = 17;
        final int height = 12;
        final int birdX = 153;
        final int birdUpX = 170;

        birdDown = new TextureRegion(gameTextures, GAME_WIDTH, 0, width,
                height);
        bird = new TextureRegion(gameTextures, birdX, 0, width, height);
        birdUp = new TextureRegion(gameTextures, birdUpX, 0, width, height);
        birdDown.flip(false, true);
        bird.flip(false, true);
        birdUp.flip(false, true);
    }

    private static void loadBirdAnimiation() {
        final float frameDuration = 0.06f;

        final TextureRegion[] birds = { birdDown, bird, birdUp };
        birdAnimation = new Animation(frameDuration, birds);
        birdAnimation.setPlayMode(Animation.PlayMode.LOOP_PINGPONG);
    }

    private static void loadSkull() {
        final int skullUpPosX = 192;
        final int width = 24;
        final int height = 14;

        skullUp = new TextureRegion(gameTextures, skullUpPosX, 0, width,
                height);
        // Create by flipping existing skullUp
        skullDown = new TextureRegion(skullUp);
        skullDown.flip(false, true);
    }

    private static void loadBar() {
        final int barY = 16;
        final int width = 22;
        final int height = 3;

        bar = new TextureRegion(gameTextures, GAME_WIDTH, barY, width,
                height);
        bar.flip(false, true);
    }

    private static void loadAudio() {
        dead = Gdx.audio.newSound(Gdx.files.internal("data/dead.wav"));
        flap = Gdx.audio.newSound(Gdx.files.internal("data/flap.wav"));
        coin = Gdx.audio.newSound(Gdx.files.internal("data/coin.wav"));
    }

    private static void loadFonts() {
        final float scaleFactor = .25f;

        font = new BitmapFont(Gdx.files.internal("data/text.fnt"));
        font.setScale(scaleFactor, -scaleFactor);
        shadow = new BitmapFont(Gdx.files.internal("data/shadow.fnt"));
        shadow.setScale(scaleFactor, -scaleFactor);
    }

    private static void loadPreferencesFile() {
        // Create (or retrieve existing) preferences file
        prefs = Gdx.app.getPreferences("ZombieBird");
        // Provide default high score of 0
        if (!prefs.contains("highScore")) {
            prefs.putInteger("highScore", 0);
        }
    }

    public static void dispose() {
        gameTextures.dispose();
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

    public static int getHighScore() {
        return prefs.getInteger("highScore");
    }

    public static void setHighScore(final int val) {
        prefs.putInteger("highScore", val);
        prefs.flush();
    }

    public static TextureRegion getLogo() {
        return logo;
    }

}
