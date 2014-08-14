package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.audio.Sound;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.Texture.TextureFilter;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.TextureRegion;
import com.kilobolt.GameWorld.GameRenderer;

public final class AssetLoader {
    private static Texture gameTextures, logoTexture;
    private static Animation birdAnimation;
    private static TextureRegion ready, retry, gameOver, scoreboard, star,
            highScoreLogo, logo, zbLogo, bg, grass, bird, birdDown, birdUp,
            skull,
            skullDown, skullUp, bar, playButtonUp, playButtonDown, noStar;
    private static Sound dead, flap, coin, fall;
    private static BitmapFont font, whiteFont, shadow;
    private static Preferences prefs;

    private AssetLoader() {
        throw new AssertionError("Instantiating utility class...");
    }

    public static void load() {
        loadKiloBoltLogo();
        loadGameTexture();
        loadPlayButtons();
        loadInGameSigns();
        loadScoreboard();
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

    private static void loadKiloBoltLogo() {
        final int width = 512;
        final int height = 114;

        logoTexture = new Texture(Gdx.files.internal("data/logo.png"));
        logoTexture.setFilter(TextureFilter.Linear, TextureFilter.Linear);

        logo = new TextureRegion(logoTexture, 0, 0, width, height);
    }

    private static void loadGameTexture() {
        gameTextures = new Texture(Gdx.files.internal("data/texture.png"));
        gameTextures.setFilter(TextureFilter.Nearest, TextureFilter.Nearest);
    }

    private static void loadPlayButtons() {
        final int y = 83;
        final int downX = 29;
        final int width = 29;
        final int height = 16;

        playButtonUp = new TextureRegion(gameTextures, 0, y, width, height);
        playButtonDown = new TextureRegion(gameTextures, downX, y,
                width, height);
        playButtonUp.flip(false, true);
        playButtonDown.flip(false, true);
    }

    private static void loadInGameSigns() {
        final int x = 59;
        final int height = 7;
        final int readyY = 83;
        final int readyWidth = 34;
        ready = new TextureRegion(gameTextures, x, readyY, readyWidth, height);
        ready.flip(false, true);

        final int retryY = 110;
        final int retryWidth = 33;
        retry = new TextureRegion(gameTextures, x, retryY, retryWidth, height);
        retry.flip(false, true);

        final int overY = 92;
        final int overWidth = 46;
        gameOver = new TextureRegion(gameTextures, x, overY, overWidth, height);
        gameOver.flip(false, true);
    }

    private static void loadScoreboard() {
        final int sbX = 111;
        final int sbY = 83;
        final int sbWidth = 97;
        final int sbHeight = 37;
        scoreboard = new TextureRegion(gameTextures, sbX, sbY, sbWidth,
                sbHeight);
        scoreboard.flip(false, true);

        final int starSideSize = 10;
        final int starY = 70;
        final int starX = 152;
        final int noStarX = 165;
        star = new TextureRegion(gameTextures, starX, starY, starSideSize,
                starSideSize);
        noStar = new TextureRegion(gameTextures, noStarX, starY, starSideSize,
                starSideSize);

        star.flip(false, true);
        noStar.flip(false, true);

        final int hsX = 59;
        final int hsY = 101;
        final int hsWidth = 48;
        final int hsHeight = 7;
        highScoreLogo = new TextureRegion(gameTextures, hsX, hsY, hsWidth,
                hsHeight);
        highScoreLogo.flip(false, true);
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

        bg = new TextureRegion(gameTextures, 0, 0, GameRenderer.GAME_WIDTH,
                height);
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

        birdDown = new TextureRegion(gameTextures, GameRenderer.GAME_WIDTH,
                0, width, height);
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

        bar = new TextureRegion(gameTextures, GameRenderer.GAME_WIDTH,
                barY, width, height);
        bar.flip(false, true);
    }

    private static void loadAudio() {
        dead = Gdx.audio.newSound(Gdx.files.internal("data/dead.wav"));
        flap = Gdx.audio.newSound(Gdx.files.internal("data/flap.wav"));
        coin = Gdx.audio.newSound(Gdx.files.internal("data/coin.wav"));
        fall = Gdx.audio.newSound(Gdx.files.internal("data/fall.wav"));
    }

    private static void loadFonts() {
        final float scaleFactor = .25f;
        final float scaleFactor2 = 0.1f;

        font = new BitmapFont(Gdx.files.internal("data/text.fnt"));
        font.setScale(scaleFactor, -scaleFactor);
        whiteFont = new BitmapFont(Gdx.files.internal("data/whitetext.fnt"));
        whiteFont.setScale(scaleFactor2, -scaleFactor2);
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
        fall.dispose();
        font.dispose();
        whiteFont.dispose();
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

    public static TextureRegion getPlayButtonUp() {
        return playButtonUp;
    }

    public static TextureRegion getPlayButtonDown() {
        return playButtonDown;
    }

    public static TextureRegion getLogo() {
        return logo;
    }

    public static TextureRegion getZbLogo() {
        return zbLogo;
    }

    public static TextureRegion getReady() {
        return ready;
    }

    public static TextureRegion getRetry() {
        return retry;
    }

    public static TextureRegion getGameOver() {
        return gameOver;
    }

    public static TextureRegion getScoreboard() {
        return scoreboard;
    }

    public static TextureRegion getHighScoreLogo() {
        return highScoreLogo;
    }

    public static TextureRegion getStar() {
        return star;
    }

    public static TextureRegion getNoStar() {
        return noStar;
    }

    public static BitmapFont getWhiteFont() {
        return whiteFont;
    }

}
