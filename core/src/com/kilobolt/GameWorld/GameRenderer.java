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
import com.kilobolt.ZBHelpers.AssetLoader;

public class GameRenderer {

    private OrthographicCamera cam;
    private ShapeRenderer shapeRenderer;
    private SpriteBatch batcher;
    private GameWorld myWorld;

    private int midPointY;
    private int gameHeight;

    // Game Objects
    private Bird bird;
    // private ScrollHandler scroller;
    // private Grass frontGrass, backGrass;
    // private Pipe pipe1, pipe2, pipe3;

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
        determineBirdStateAndDraw(runTime);
        batcher.end();
    }

    private void drawBackground() {
        batcher.disableBlending();
        batcher.draw(background, 0, midPointY + 23, GAME_WIDTH, 43);
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
