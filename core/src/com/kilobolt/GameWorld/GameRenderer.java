package com.kilobolt.GameWorld;

import com.badlogic.gdx.Gdx;
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
import com.kilobolt.ZBHelpers.AssetLoader;

public class GameRenderer {
    private OrthographicCamera cam;
    private ShapeRenderer shapeRenderer;
    private SpriteBatch batcher;
    private final GameWorld myWorld;
    private final int midPointY;
    private final int gameHeight;
    // Game Objects
    private Bird bird;
    private ScrollHandler scroller;
    private Grass frontGrass, backGrass;
    private Pipe pipe1, pipe2, pipe3;
    // Game Assets
    private TextureRegion background, grass;
    private Animation birdAnimation;
    private TextureRegion birdMid, birdDown, birdUp;
    private TextureRegion skullUp, skullDown, bar;
    private static final int GAME_WIDTH = 136;

    public GameRenderer(GameWorld world, int gameHeight, int midPointY) {
        myWorld = world;
        this.gameHeight = gameHeight;
        this.midPointY = midPointY;
        intializeCam(gameHeight);
        initializeBatcher();
        initializeShapeRenderer();
        initializeGameObjects();
        initializeAssets();
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
        bird = myWorld.getBird();
        scroller = myWorld.getScroller();
        frontGrass = scroller.getFrontGrass();
        backGrass = scroller.getBackGrass();
        pipe1 = scroller.getPipe1();
        pipe2 = scroller.getPipe2();
        pipe3 = scroller.getPipe3();
    }

    private void initializeAssets() {
        background = AssetLoader.bg;
        grass = AssetLoader.grass;
        birdAnimation = AssetLoader.birdAnimation;
        birdMid = AssetLoader.bird;
        birdDown = AssetLoader.birdDown;
        birdUp = AssetLoader.birdUp;
        skullUp = AssetLoader.skullUp;
        skullDown = AssetLoader.skullDown;
        bar = AssetLoader.bar;
    }

    public void render(float runTime) {
        preventPotentialScreenFlicker();
        renderShapeObjects();
        renderBatchObjects(runTime);
    }

    private void preventPotentialScreenFlicker() {
        Gdx.graphics.getGL20().glClearColor(0, 0, 0, 1);
        Gdx.graphics.getGL20().glClear(GL20.GL_COLOR_BUFFER_BIT);
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
        shapeRenderer.rect(0, 0, GAME_WIDTH, midPointY + 66);
    }

    private void drawGrassShape() {
        shapeRenderer.setColor(111 / 255.0f, 186 / 255.0f, 45 / 255.0f, 1);
        shapeRenderer.rect(0, midPointY + 66, 136, 11);
    }

    private void drawDirtShape() {
        shapeRenderer.setColor(147 / 255.0f, 80 / 255.0f, 27 / 255.0f, 1);
        shapeRenderer.rect(0, midPointY + 77, 136, 52);
    }

    private void renderBatchObjects(float runTime) {
        batcher.begin();
        drawBackground();
        drawGrass();
        drawPipes();
        drawSkulls();
        determineBirdStateAndDraw(runTime);
        batcher.end();
    }

    private void drawBackground() {
        batcher.disableBlending();
        batcher.draw(background, 0, midPointY + 23, GAME_WIDTH, 43);
    }

    private void drawGrass() {
        // Draw the grass
        batcher.draw(grass, frontGrass.getX(), frontGrass.getY(),
                frontGrass.getWidth(), frontGrass.getHeight());
        batcher.draw(grass, backGrass.getX(), backGrass.getY(),
                backGrass.getWidth(), backGrass.getHeight());
    }

    private void drawPipes() {
        // Temporary code! Sorry about the mess :)
        batcher.draw(bar, pipe1.getX(), pipe1.getY(), pipe1.getWidth(),
                pipe1.getHeight());
        batcher.draw(bar, pipe1.getX(), pipe1.getY() + pipe1.getHeight() + 45,
                pipe1.getWidth(), midPointY + 66 - (pipe1.getHeight() + 45));
        batcher.draw(bar, pipe2.getX(), pipe2.getY(), pipe2.getWidth(),
                pipe2.getHeight());
        batcher.draw(bar, pipe2.getX(), pipe2.getY() + pipe2.getHeight() + 45,
                pipe2.getWidth(), midPointY + 66 - (pipe2.getHeight() + 45));
        batcher.draw(bar, pipe3.getX(), pipe3.getY(), pipe3.getWidth(),
                pipe3.getHeight());
        batcher.draw(bar, pipe3.getX(), pipe3.getY() + pipe3.getHeight() + 45,
                pipe3.getWidth(), midPointY + 66 - (pipe3.getHeight() + 45));
    }

    private void drawSkulls() {
        // Temporary code! Sorry about the mess :)
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

    private void determineBirdStateAndDraw(float runTime) {
        TextureRegion birdState;
        if (bird.isFlapping())
            birdState = birdAnimation.getKeyFrame(runTime);
        else
            birdState = birdMid;
        drawBird(birdState);
    }

    private void drawBird(TextureRegion birdState) {
        batcher.enableBlending();
        batcher.draw(birdState, bird.getX(), bird.getY(),
                bird.getWidth() / 2.0f, bird.getHeight() / 2.0f,
                bird.getWidth(), bird.getHeight(), 1, 1, bird.getRotation());
    }
}
