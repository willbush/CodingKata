package kiloboltgame;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;

import kiloboltgame.framework.Animation;

public class StartingClass extends Applet implements Runnable, KeyListener {

    enum GameState {
        DEAD, RUNNING
    }

    GameState state = GameState.RUNNING;

    public static Image tilegrassTop, tilegrassBot, tilegrassLeft,
            tilegrassRight, tiledirt;
    public static int score = 0;

    private Font font = new Font(null, Font.BOLD, 30);
    private static Background bg1, bg2;
    private static final long serialVersionUID = 1L;
    private boolean debugMode = false;

    private static Robot robot;
    public static Heliboy hb, hb2;
    private Animation robotAnim, heliAnim;
    private Graphics second;
    private URL base;
    private Image image, robotImageState, robotStanding, robotStanding2,
            robotStanding3, robotDucking, robotJumping, background, heliboy,
            heliboy2, heliboy3, heliboy4, heliboy5;
    private ArrayList<Tile> tilearray = new ArrayList<Tile>();

    @Override
    public void init() {
        setupFrame();
        setupImages();
        setupAnimations();
    }

    private void setupFrame() {
        setSize(800, 480);
        setBackground(Color.BLACK);
        setFocusable(true);
        addKeyListener(this);
        Frame frame = (Frame) this.getParent().getParent();
        frame.setTitle("Q-Bot Alpha");
        try {
            base = getDocumentBase();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setupImages() {
        robotImages();
        enemyImages();
        environmentImages();
    }

    private void environmentImages() {
        background = getImage(base, "data/background.png");
        tiledirt = getImage(base, "data/tiledirt.png");
        tilegrassTop = getImage(base, "data/tilegrasstop.png");
        tilegrassBot = getImage(base, "data/tilegrassbot.png");
        tilegrassLeft = getImage(base, "data/tilegrassleft.png");
        tilegrassRight = getImage(base, "data/tilegrassright.png");
    }

    private void enemyImages() {
        heliboy = getImage(base, "data/heliboy.png");
        heliboy2 = getImage(base, "data/heliboy2.png");
        heliboy3 = getImage(base, "data/heliboy3.png");
        heliboy4 = getImage(base, "data/heliboy4.png");
        heliboy5 = getImage(base, "data/heliboy5.png");
    }

    private void robotImages() {
        robotStanding = getImage(base, "data/character.png");
        robotStanding2 = getImage(base, "data/character2.png");
        robotStanding3 = getImage(base, "data/character3.png");

        robotDucking = getImage(base, "data/duck.png");
        robotJumping = getImage(base, "data/jumped.png");
    }

    private void setupAnimations() {
        robotAnimation();
        heliboyAnimation();
    }

    private void heliboyAnimation() {
        heliAnim = new Animation();
        heliAnim.addFrame(heliboy, 100);
        heliAnim.addFrame(heliboy2, 100);
        heliAnim.addFrame(heliboy3, 100);
        heliAnim.addFrame(heliboy4, 100);
        heliAnim.addFrame(heliboy5, 100);
        heliAnim.addFrame(heliboy4, 100);
        heliAnim.addFrame(heliboy3, 100);
        heliAnim.addFrame(heliboy2, 100);
    }

    private void robotAnimation() {
        robotAnim = new Animation();
        robotAnim.addFrame(robotStanding, 1250);
        robotAnim.addFrame(robotStanding2, 50);
        robotAnim.addFrame(robotStanding3, 50);
        robotAnim.addFrame(robotStanding2, 50);

        robotImageState = robotAnim.getImage();
    }

    @Override
    public void start() {
        bg1 = new Background(0, 0);
        bg2 = new Background(2160, 0);
        robot = new Robot();
        initializeTiles();
        hb = new Heliboy(340, 360);
        hb2 = new Heliboy(700, 370);
        Thread thread = new Thread(this);
        thread.start();
    }

    private void initializeTiles() {
        try {
            loadMap("data/map1.txt");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void loadMap(String filename) throws IOException {
        ArrayList<String> lines = new ArrayList<String>();
        int width = 0;

        BufferedReader reader = new BufferedReader(new FileReader(filename));

        while (true) {
            String line = reader.readLine();

            if (line == null) {
                reader.close();
                break;
            }
            if (!line.startsWith("!")) {
                lines.add(line);
                width = Math.max(width, line.length());
            }

        }

        for (int y = 0; y < 12; y++) {
            String line = (String) lines.get(y);
            for (int x = 0; x < width; x++) {
                if (x < line.length()) {
                    char ch = line.charAt(x);
                    Tile t = new Tile(x, y, Character.getNumericValue(ch));
                    tilearray.add(t);
                }
            }
        }
    }

    @Override
    public void stop() {
        // TODO Auto-generated method stub
        super.stop();
    }

    @Override
    public void destroy() {
        // TODO Auto-generated method stub
        super.destroy();
    }

    @Override
    public void run() {
        if (state == GameState.RUNNING) {
            while (true) {
                checkGameState();
                robot.update();
                handleRobotImageState();
                handleProjectiles();
                updateTiles();
                hb.update();
                hb2.update();
                bg1.update();
                bg2.update();
                animate();
                repaint();
                keepGamePace();
            }
        }
    }

    private void checkGameState() {
        if (robot.getRobotPosY() > 500)
            state = GameState.DEAD;
    }

    /**
     * The intention here to is slow down the run method with sleep in order to
     * create a playable game pace.
     */
    private void keepGamePace() {
        try {
            Thread.sleep(17);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void animate() {
        robotAnim.update(10);
        heliAnim.update(50);
    }

    private void updateTiles() {
        for (int i = 0; i < tilearray.size(); i++) {
            Tile t = (Tile) tilearray.get(i);
            t.update();
        }
    }

    private void handleProjectiles() {
        ArrayList<Projectile> projectiles = robot.getProjectiles();
        for (int i = 0; i < projectiles.size(); i++) {
            Projectile p = (Projectile) projectiles.get(i);
            if (p.isVisible())
                p.update();
            else
                projectiles.remove(i);
        }
    }

    private void handleRobotImageState() {
        if (robot.isJumped())
            robotImageState = robotJumping;
        else if (robot.isDucked())
            robotImageState = robotDucking;
        else if (robot.isJumped() == false && robot.isDucked() == false)
            robotImageState = robotAnim.getImage();
    }

    @Override
    public void update(Graphics g) {
        if (image == null) {
            image = createImage(this.getWidth(), this.getHeight());
            second = image.getGraphics();
        }

        second.setColor(getBackground());
        second.fillRect(0, 0, getWidth(), getHeight());
        second.setColor(getForeground());
        paint(second);

        g.drawImage(image, 0, 0, this);
    }

    @Override
    public void paint(Graphics g) {
        if (state == GameState.RUNNING) {
            drawBackground(g);
            paintTiles(g);
            drawProjectiles(g);
            drawCollisionBox(g);
            drawRobot(g);
            drawHeliboy(g);
            drawScore(g);
        } else if (state == GameState.DEAD) {
            g.setColor(Color.BLACK);
            g.fillRect(0, 0, 840, 480);
            g.setColor(Color.WHITE);
            g.drawString("Game Over", 310, 240);
        }
    }

    private void drawCollisionBox(Graphics g) {
        if (debugMode == true) {
            g.drawRect((int) Robot.headBox.getX(), (int) Robot.headBox.getY(),
                    (int) Robot.headBox.getWidth(),
                    (int) Robot.headBox.getHeight());
            g.drawRect((int) Robot.rightTorsoBox.getX(),
                    (int) Robot.rightTorsoBox.getY(),
                    (int) Robot.rightTorsoBox.getWidth(),
                    (int) Robot.rightTorsoBox.getHeight());
            g.drawRect((int) Robot.leftTorsoBox.getX(),
                    (int) Robot.leftTorsoBox.getY(),
                    (int) Robot.leftTorsoBox.getWidth(),
                    (int) Robot.leftTorsoBox.getHeight());
            g.drawRect((int) Robot.rightHandBox.getX(),
                    (int) Robot.rightHandBox.getY(),
                    (int) Robot.rightHandBox.getWidth(),
                    (int) Robot.rightHandBox.getHeight());
            g.drawRect((int) Robot.leftHandBox.getX(),
                    (int) Robot.leftHandBox.getY(),
                    (int) Robot.leftHandBox.getWidth(),
                    (int) Robot.leftHandBox.getHeight());
            g.drawRect((int) Robot.feetBox.getX(), (int) Robot.feetBox.getY(),
                    (int) Robot.feetBox.getWidth(),
                    (int) Robot.feetBox.getHeight());
            g.drawRect((int) Robot.checkCollisionBox.getX(),
                    (int) Robot.checkCollisionBox.getY(),
                    (int) Robot.checkCollisionBox.getWidth(),
                    (int) Robot.checkCollisionBox.getHeight());
        }
    }

    private void drawBackground(Graphics g) {
        g.drawImage(background, bg1.getBgX(), bg1.getBgY(), this);
        g.drawImage(background, bg2.getBgX(), bg2.getBgY(), this);
    }

    private void paintTiles(Graphics g) {
        for (int i = 0; i < tilearray.size(); i++) {
            Tile t = (Tile) tilearray.get(i);
            g.drawImage(t.getTileImage(), t.getTilePosX(), t.getTilePosY(),
                    this);
        }
    }

    private void drawProjectiles(Graphics g) {
        ArrayList<Projectile> projectiles = robot.getProjectiles();
        for (int i = 0; i < projectiles.size(); i++) {
            Projectile p = (Projectile) projectiles.get(i);
            g.setColor(Color.YELLOW);
            g.fillRect(p.getxPosition(), p.getyPosition(), 10, 5);
        }
    }

    private void drawRobot(Graphics g) {
        g.drawImage(robotImageState, robot.getRobotPosX() - 61,
                robot.getRobotPosY() - 63, this);
    }

    private void drawHeliboy(Graphics g) {
        g.drawImage(heliAnim.getImage(), hb.getEnemyPosX() - 48,
                hb.getEnemyPosY() - 48, this);
        g.drawImage(heliAnim.getImage(), hb2.getEnemyPosX() - 48,
                hb2.getEnemyPosY() - 48, this);
    }

    private void drawScore(Graphics g) {
        g.setFont(font);
        g.setColor(Color.WHITE);
        g.drawString(Integer.toString(score), 740, 30);
    }

    @Override
    public void keyPressed(KeyEvent e) {
        switch (e.getKeyCode()) {
        case KeyEvent.VK_UP:
        case KeyEvent.VK_W:
            System.out.println("move up");
            break;
        case KeyEvent.VK_DOWN:
        case KeyEvent.VK_S:
            if (robot.isJumped() == false) {
                robot.setDucked(true);
                robot.setRobotSpeedX(0);
            }
            break;
        case KeyEvent.VK_LEFT:
        case KeyEvent.VK_A:
            robot.moveLeft();
            robot.setMovingLeft(true);
            break;
        case KeyEvent.VK_RIGHT:
        case KeyEvent.VK_D:
            robot.moveRight();
            robot.setMovingRight(true);
            break;
        case KeyEvent.VK_SPACE:
            robot.jump();
            break;
        case KeyEvent.VK_CONTROL:
            if (robot.isDucked() == false) {
                robot.shoot();
                robot.setReadyToFire(false);
            }
            break;
        }
    }

    @Override
    public void keyReleased(KeyEvent e) {
        switch (e.getKeyCode()) {
        case KeyEvent.VK_UP:
        case KeyEvent.VK_W:
            System.out.println("Stop moving up");
            break;
        case KeyEvent.VK_DOWN:
        case KeyEvent.VK_S:
            robotImageState = robotAnim.getImage();
            robot.setDucked(false);
            break;
        case KeyEvent.VK_LEFT:
        case KeyEvent.VK_A:
            robot.stopLeft();
            break;
        case KeyEvent.VK_RIGHT:
        case KeyEvent.VK_D:
            robot.stopRight();
            break;
        case KeyEvent.VK_SPACE:
            break;
        case KeyEvent.VK_CONTROL:
            robot.setReadyToFire(true);
        }
    }

    @Override
    public void keyTyped(KeyEvent e) {
        // TODO Auto-generated method stub
    }

    public static Background getBg1() {
        return bg1;
    }

    public static Background getBg2() {
        return bg2;
    }

    public static Robot getRobot() {
        return robot;
    }
}