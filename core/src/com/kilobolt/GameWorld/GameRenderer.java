package com.kilobolt.GameWorld;

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
import com.kilobolt.ZBHelpers.AssetLoader;

public class GameRenderer {
    private OrthographicCamera cam;
    private ShapeRenderer shapeRenderer;
    private SpriteBatch batcher;
    private final GameWorld myWorld;
    private final int myMidPointY;
    private final int myGameHeight;
    private Bird bird;
    private ScrollHandler scroller;
    private Grass frontGrass, backGrass;
    private Pipe pipe1, pipe2, pipe3;
    private TextureRegion background, grass;
    private Animation birdAnimation;
    private TextureRegion birdMid, birdDown, birdUp;
    private TextureRegion skullUp, skullDown, bar;
    private static final int GAME_WIDTH = 136;

    public GameRenderer(final GameWorld world, final int gameHeight,
            final int midPointY) {
        myWorld = world;
        myGameHeight = gameHeight;
        myMidPointY = midPointY;
        intializeCam(gameHeight);
        initializeBatcher();
        initializeShapeRenderer();
        initializeGameObjects();
        initializeAssets();
    }

    private void intializeCam(final int gameHeight) {
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
        bird = myWorld.getBird();
        scroller = myWorld.getScroller();
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
        birdDown = AssetLoader.getBirdDown();
        birdUp = AssetLoader.getBirdUp();
        skullUp = AssetLoader.getSkullUp();
        skullDown = AssetLoader.getSkullDown();
        bar = AssetLoader.getBar();
    }

    public final void render(final float runTime) {
        renderShapeObjects();
        renderBatchObjects(runTime);
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

    private void renderBatchObjects(final float runTime) {
        batcher.begin();
        drawBackground();
        drawGrass();
        drawPipes();
        drawSkulls();
        determineBirdStateAndDraw(runTime);
        drawText();
        batcher.end();
    }

    private void drawText() {
        // temporary code, will fix later.
        if (myWorld.isReady()) {
            AssetLoader.getShadow().draw(batcher, "Touch me", (136 / 2) - (42),
                    76);
            AssetLoader.getFont()
                    .draw(batcher, "Touch me", (136 / 2) - (42 - 1), 75);
        } else {
            if (myWorld.isGameOver() || myWorld.isHighScore()) {
                if (myWorld.isGameOver()) {
                    AssetLoader.getShadow().draw(batcher, "Game Over", 25, 56);
                    AssetLoader.getFont().draw(batcher, "Game Over", 24, 55);
                    AssetLoader.getShadow().draw(batcher, "High Score:", 23,
                            106);
                    AssetLoader.getFont().draw(batcher, "High Score:", 22, 105);
                    String highScore = AssetLoader.getHighScore() + "";
                    AssetLoader.getShadow().draw(batcher, highScore, (136 / 2)
                            - (3 * highScore.length()), 128);
                    AssetLoader.getFont().draw(batcher, highScore, (136 / 2)
                            - (3 * highScore.length() - 1), 127);
                } else {
                    AssetLoader.getShadow()
                            .draw(batcher, "High Score!", 19, 56);
                    AssetLoader.getFont().draw(batcher, "High Score!", 18, 55);
                }
                AssetLoader.getShadow().draw(batcher, "Try again?", 23, 76);
                AssetLoader.getFont().draw(batcher, "Try again?", 24, 75);
                String score = myWorld.getScore() + "";
                AssetLoader.getShadow().draw(batcher, score,
                        (136 / 2) - (3 * score.length()), 12);
                AssetLoader.getFont().draw(batcher, score,
                        (136 / 2) - (3 * score.length() - 1), 11);
            }
            String score = myWorld.getScore() + "";
            AssetLoader.getShadow().draw(batcher, "" + myWorld.getScore(),
                    (136 / 2)
                            - (3 * score.length()), 12);
            AssetLoader.getFont().draw(batcher, "" + myWorld.getScore(),
                    (136 / 2)
                            - (3 * score.length() - 1), 11);
        }
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
        batcher.draw(skullUp, pipe1.getX() - 1,
                pipe1.getY() + pipe1.getHeight() - 14, 24, 14);
        batcher.draw(skullDown, pipe1.getX() - 1,
                pipe1.getY() + pipe1.getHeight() + 45, 24, 14);
        batcher.draw(skullUp, pipe2.getX() - 1,
                pipe2.getY() + pipe2.getHeight() - 14, 24, 14);
        batcher.draw(skullDown, pipe2.getX() - 1,
                pipe2.getY() + pipe2.getHeight() + 45, 24, 14);
        batcher.draw(skullUp, pipe3.getX() - 1,
                pipe3.getY() + pipe3.getHeight() - 14, 24, 14);
        batcher.draw(skullDown, pipe3.getX() - 1,
                pipe3.getY() + pipe3.getHeight() + 45, 24, 14);
    }

    private void determineBirdStateAndDraw(final float runTime) {
        TextureRegion birdState;
        if (bird.isFlapping()) {
            birdState = birdAnimation.getKeyFrame(runTime);
        } else {
            birdState = birdMid;
        }
        drawBird(birdState);
    }

    private void drawBird(final TextureRegion birdState) {
        batcher.enableBlending();
        batcher.draw(birdState, bird.getX(), bird.getY(),
                bird.getWidth() / 2.0f, bird.getHeight() / 2.0f,
                bird.getWidth(), bird.getHeight(), 1, 1, bird.getRotation());
    }
}
