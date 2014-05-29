package com.kilobolt.ZBHelpers;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.Texture.TextureFilter;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.TextureRegion;

public class AssetLoader {
    public static Texture texture;
    public static TextureRegion bg, grass;
    public static Animation birdAnimation;
    public static TextureRegion bird, birdDown, birdUp;
    public static TextureRegion skull, skullDown, skullUp, bar;
    private static final int GAME_WIDTH = 136;

    public static void load() {
        texture = new Texture(Gdx.files.internal("data/texture.png"));
        texture.setFilter(TextureFilter.Nearest, TextureFilter.Nearest);
        bg = new TextureRegion(texture, 0, 0, GAME_WIDTH, 43);
        // flipping because we are using "Y down" coordinate system.
        bg.flip(false, true);
        grass = new TextureRegion(texture, 0, 43, 143, 11);
        grass.flip(false, true);
        birdDown = new TextureRegion(texture, GAME_WIDTH, 0, 17, 12);
        birdDown.flip(false, true);
        bird = new TextureRegion(texture, 153, 0, 17, 12);
        bird.flip(false, true);
        birdUp = new TextureRegion(texture, 170, 0, 17, 12);
        birdUp.flip(false, true);
        final TextureRegion[] birds = { birdDown, bird, birdUp };
        birdAnimation = new Animation(0.06f, birds);
        birdAnimation.setPlayMode(Animation.PlayMode.LOOP_PINGPONG);
        skullUp = new TextureRegion(texture, 192, 0, 24, 14);
        // Create by flipping existing skullUp
        skullDown = new TextureRegion(skullUp);
        skullDown.flip(false, true);
        bar = new TextureRegion(texture, GAME_WIDTH, 16, 22, 3);
        bar.flip(false, true);
    }

    public static void dispose() {
        texture.dispose();
    }
}
