package com.kilobolt.GameWorld;

import java.util.List;

import aurelienribon.tweenengine.Tween;
import aurelienribon.tweenengine.TweenEquations;
import aurelienribon.tweenengine.TweenManager;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.g2d.TextureRegion;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType;
import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameObjects.Grass;
import com.kilobolt.GameObjects.Pipe;
import com.kilobolt.GameObjects.ScrollHandler;
import com.kilobolt.TweenAccessors.Value;
import com.kilobolt.TweenAccessors.ValueAccessor;
import com.kilobolt.UI.SimpleButton;
import com.kilobolt.ZBHelpers.AssetLoader;
import com.kilobolt.ZBHelpers.InputHandler;

public final class GameRenderer {
    public static final int GAME_WIDTH = 136;
    private OrthographicCamera cam;
    private ShapeRenderer shapeRenderer;
    private SpriteBatch batcher;
    private final GameWorld world;
    private final int myMidPointY;

    // game objects
    private Bird bird;
    private ScrollHandler scroller;
    private Grass frontGrass, backGrass;
    private Pipe pipe1, pipe2, pipe3;

    // game assets
    private Animation birdAnimation;
    private TextureRegion background, grass, birdMid, skullUp, skullDown, bar,
            ready, zbLogo, gameOver, highScore, scoreboard, star, noStar,
            retry;

    // tween stuff
    private TweenManager manager;
    private final Value alpha = new Value();

    // buttons
    private List<SimpleButton> menuButtons;
    private Color transitionColor;

    public GameRenderer(GameWorld gw, int gameHeight, int midPointY) {
        world = gw;
        myMidPointY = midPointY;

        initializeMenuButtons();
        intializeCam(gameHeight);
        initializeBatcher();
        initializeShapeRenderer();
        initializeGameObjects();
        initializeAssets();
        setupTransition();
    }

    private void initializeMenuButtons() {
        this.menuButtons = ((InputHandler) Gdx.input.getInputProcessor())
                .getMenuButtons();
    }

    private void intializeCam(int gameHeight) {
        cam = new OrthographicCamera();
        cam.setToOrtho(true, GAME_WIDTH, gameHeight);
    }

    private void initializeBatcher() {
        batcher = new SpriteBatch();
        batcher.setProjectionMatrix(cam.combined);
    }

    private void initializeShapeRenderer() {
        shapeRenderer = new ShapeRenderer();
        shapeRenderer.setProjectionMatrix(cam.combined);
    }

    private void initializeGameObjects() {
        bird = world.getBird();
        scroller = world.getScroller();
        frontGrass = scroller.getFrontGrass();
        backGrass = scroller.getBackGrass();
        pipe1 = scroller.getPipe1();
        pipe2 = scroller.getPipe2();
        pipe3 = scroller.getPipe3();
    }

    private void initializeAssets() {
        background = AssetLoader.getBg();
        grass = AssetLoader.getGrass();
        birdAnimation = AssetLoader.getBirdAnimation();
        birdMid = AssetLoader.getBird();
        skullUp = AssetLoader.getSkullUp();
        skullDown = AssetLoader.getSkullDown();
        bar = AssetLoader.getBar();
        ready = AssetLoader.getReady();
        zbLogo = AssetLoader.getZbLogo();
        gameOver = AssetLoader.getGameOver();
        highScore = AssetLoader.getHighScoreLogo();
        scoreboard = AssetLoader.getScoreboard();
        retry = AssetLoader.getRetry();
        star = AssetLoader.getStar();
        noStar = AssetLoader.getNoStar();
    }

    private void setupTransition() {
        transitionColor = new Color();
        transitionColor.set(1.0f, 1.0f, 1.0f, 1);
        alpha.setValue(1);
        Tween.registerAccessor(Value.class, new ValueAccessor());
        manager = new TweenManager();
        final float duration = .5f;
        Tween.to(alpha, -1, duration).target(0)
                .ease(TweenEquations.easeOutQuad)
                .start(manager);
    }

    public void render(float delta, float runTime) {
        clearScreenAndBuffer();
        renderShapeObjects();
        renderBatchObjects(runTime);
        drawTransition(delta);
    }

    private void clearScreenAndBuffer() {
        Gdx.graphics.getGL20().glClearColor(0, 0, 0, 1);
        Gdx.graphics.getGL20().glClear(
                GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT);
    }

    private void renderShapeObjects() {
        shapeRenderer.begin(ShapeType.Filled);
        drawBackgroundColor();
        drawGrassShape();
        drawDirtShape();
        shapeRenderer.end();
    }

    private void drawBackgroundColor() {
        shapeRenderer.setColor(55 / 255.0f, 80 / 255.0f, 100 / 255.0f, 1);
        shapeRenderer.rect(0, 0, GAME_WIDTH, myMidPointY + 66);
    }

    private void drawGrassShape() {
        shapeRenderer.setColor(111 / 255.0f, 186 / 255.0f, 45 / 255.0f, 1);
        shapeRenderer.rect(0, myMidPointY + 66, 136, 11);
    }

    private void drawDirtShape() {
        shapeRenderer.setColor(147 / 255.0f, 80 / 255.0f, 27 / 255.0f, 1);
        shapeRenderer.rect(0, myMidPointY + 77, 136, 52);
    }

    private void renderBatchObjects(float runTime) {
        batcher.begin();
        drawBackground();
        drawGrass();
        drawPipes();
        drawSkulls();
        drawBirdCurrentState(findBirdState(runTime));

        if (world.isRunning()) {
            drawScore();
        } else if (world.isReady()) {
            drawReady();
        } else if (world.isMenu()) {
            drawMenuUI();
        } else if (world.isGameOver()) {
            drawScoreboard();
            drawGameOver();
            drawRetry();
        } else if (world.isHighScore()) {
            drawScoreboard();
            drawHighScore();
            drawRetry();
        }
        batcher.end();
    }

    private void drawBackground() {
        batcher.disableBlending();
        batcher.draw(background, 0, myMidPointY + 23, GAME_WIDTH, 43);
    }

    private void drawGrass() {
        batcher.draw(grass, frontGrass.getX(), frontGrass.getY(),
                frontGrass.getWidth(), frontGrass.getHeight());
        batcher.draw(grass, backGrass.getX(), backGrass.getY(),
                backGrass.getWidth(), backGrass.getHeight());
    }

    private void drawPipes() {
        batcher.draw(bar, pipe1.getX(), pipe1.getY(), pipe1.getWidth(),
                pipe1.getHeight());
        batcher.draw(bar, pipe1.getX(), pipe1.getY() + pipe1.getHeight() + 45,
                pipe1.getWidth(), myMidPointY + 66 - (pipe1.getHeight() + 45));
        batcher.draw(bar, pipe2.getX(), pipe2.getY(), pipe2.getWidth(),
                pipe2.getHeight());
        batcher.draw(bar, pipe2.getX(), pipe2.getY() + pipe2.getHeight() + 45,
                pipe2.getWidth(), myMidPointY + 66 - (pipe2.getHeight() + 45));
        batcher.draw(bar, pipe3.getX(), pipe3.getY(), pipe3.getWidth(),
                pipe3.getHeight());
        batcher.draw(bar, pipe3.getX(), pipe3.getY() + pipe3.getHeight() + 45,
                pipe3.getWidth(), myMidPointY + 66 - (pipe3.getHeight() + 45));
    }

    private void drawSkulls() {
        final int height = 14;
        final int width = 24;
        final int heightModifier = 45;

        final float x1 = pipe1.getX() - 1;
        final float upY1 = pipe1.getY() + pipe1.getHeight() - height;
        final float downY1 = pipe1.getY() + pipe1.getHeight() + heightModifier;
        batcher.draw(skullUp, x1, upY1, width, height);
        batcher.draw(skullDown, x1, downY1, width, height);

        final float x2 = pipe2.getX() - 1;
        final float upY2 = pipe2.getY() + pipe2.getHeight() - height;
        final float downY2 = pipe2.getY() + pipe2.getHeight() + heightModifier;
        batcher.draw(skullUp, x2, upY2, width, height);
        batcher.draw(skullDown, x2, downY2, width, height);

        final float x3 = pipe3.getX() - 1;
        final float upY3 = pipe3.getY() + pipe3.getHeight() - height;
        final float downY3 = pipe3.getY() + pipe3.getHeight() + heightModifier;
        batcher.draw(skullUp, x3, upY3, width, height);
        batcher.draw(skullDown, x3, downY3, width, height);
    }

    private TextureRegion findBirdState(float runTime) {
        final TextureRegion birdState;
        if (bird.isFlapping() || world.isMenu()) {
            birdState = birdAnimation.getKeyFrame(runTime);
        } else {
            birdState = birdMid;
        }
        return birdState;
    }

    private void drawBirdCurrentState(TextureRegion birdState) {
        batcher.enableBlending();
        batcher.draw(birdState, bird.getX(), bird.getY(),
                bird.getWidth() / 2.0f, bird.getHeight() / 2.0f,
                bird.getWidth(), bird.getHeight(), 1, 1, bird.getRotation());
    }

    private void drawScore() {
        final int length = ("" + world.getScore()).length();
        AssetLoader.getShadow().draw(batcher, "" + world.getScore(),
                68 - (3 * length), myMidPointY - 82);
        AssetLoader.getFont().draw(batcher, "" + world.getScore(),
                68 - (3 * length), myMidPointY - 83);
    }

    private void drawHighScore() {
        batcher.draw(highScore, 22, myMidPointY - 50, 96, 14);
    }

    private void drawMenuUI() {
        final int x = GAME_WIDTH / 2 - 56;
        final int y = myMidPointY - 50;
        final float width = zbLogo.getRegionWidth() / 1.2f;
        final float height = zbLogo.getRegionHeight() / 1.2f;
        batcher.draw(zbLogo, x, y, width, height);

        for (final SimpleButton button : menuButtons) {
            button.draw(batcher);
        }

    }

    private void drawScoreboard() {
        batcher.draw(scoreboard, 22, myMidPointY - 30, 97, 37);

        final int squareSideLen = 10;
        final int y = myMidPointY - 15;
        final int x = 25;
        final int x2 = 37;
        final int x3 = 49;
        final int x4 = 61;
        final int x5 = 73;
        batcher.draw(noStar, x, y, squareSideLen, squareSideLen);
        batcher.draw(noStar, x2, y, squareSideLen, squareSideLen);
        batcher.draw(noStar, x3, y, squareSideLen, squareSideLen);
        batcher.draw(noStar, x4, y, squareSideLen, squareSideLen);
        batcher.draw(noStar, x5, y, squareSideLen, squareSideLen);

        if (world.getScore() > 2) {
            batcher.draw(star, x5, y, squareSideLen, squareSideLen);
        }
        if (world.getScore() > 17) {
            batcher.draw(star, x4, y, squareSideLen, squareSideLen);
        }
        if (world.getScore() > 50) {
            batcher.draw(star, x3, y, squareSideLen, squareSideLen);
        }
        if (world.getScore() > 80) {
            batcher.draw(star, x2, y, squareSideLen, squareSideLen);
        }
        if (world.getScore() > 120) {
            batcher.draw(star, x, y, squareSideLen, squareSideLen);
        }

        final int length = ("" + world.getScore()).length();

        AssetLoader.getWhiteFont().draw(batcher, "" + world.getScore(),
                104 - (2 * length), myMidPointY - 20);

        final int length2 = ("" + AssetLoader.getHighScore()).length();
        AssetLoader.getWhiteFont().draw(batcher,
                "" + AssetLoader.getHighScore(), 104 - (2.5f * length2),
                myMidPointY - 3);
    }

    private void drawRetry() {
        batcher.draw(retry, 36, myMidPointY + 10, 66, 14);
    }

    private void drawReady() {
        batcher.draw(ready, 36, myMidPointY - 50, 68, 14);
    }

    private void drawGameOver() {
        batcher.draw(gameOver, 24, myMidPointY - 50, 92, 14);
    }

    private void drawTransition(float delta) {
        if (alpha.getValue() > 0) {
            manager.update(delta);
            Gdx.gl.glEnable(GL20.GL_BLEND);
            Gdx.gl.glBlendFunc(GL20.GL_SRC_ALPHA, GL20.GL_ONE_MINUS_SRC_ALPHA);
            shapeRenderer.begin(ShapeType.Filled);
            shapeRenderer.setColor(transitionColor.r, transitionColor.g,
                    transitionColor.b, alpha.getValue());
            shapeRenderer.rect(0, 0, GAME_WIDTH, 300);
            shapeRenderer.end();
            Gdx.gl.glDisable(GL20.GL_BLEND);
        }
    }

}
