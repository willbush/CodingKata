package kiloboltgame;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URL;

public class StartingClass extends Applet implements Runnable, KeyListener {

    private static final long serialVersionUID = 1L;
    private Robot robot;
    private Heliboy hb, hb2;
    private Image image, robotImageState, robotStand, robotDuck, robotJump,
            background, heliboy;
    private Graphics second;
    private URL base;
    private static Background bg1, bg2;

    @Override
    public void init() {
        setupFrame();
        setupImage();
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
            // TODO: handle exception
        }
    }

    private void setupImage() {
        robotStand = getImage(base, "data/character.png");
        robotImageState = robotStand;
        robotDuck = getImage(base, "data/duck.png");
        robotJump = getImage(base, "data/jumped.png");
        heliboy = getImage(base, "data/heliboy.png");
        background = getImage(base, "data/background.png");
    }

    @Override
    public void start() {
        bg1 = new Background(0, 0);
        bg2 = new Background(2160, 0);
        robot = new Robot();
        hb = new Heliboy(340, 360);
        hb2 = new Heliboy(700, 370);
        Thread thread = new Thread(this);
        thread.start();
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
        while (true) {
            robot.update();
            hb.update();
            hb2.update();
            handleImageState();
            bg1.update();
            bg2.update();
            repaint();
            try {
                Thread.sleep(17);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private void handleImageState() {
        if (robot.hasJumped()) {
            robotImageState = robotJump;
        } else if (robot.isDucked()) {
            robotImageState = robotDuck;
        } else if (robot.hasJumped() == false && robot.isDucked() == false) {
            robotImageState = robotStand;
        }
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
        drawBackground(g);
        drawRobot(g);
        drawHeliboy(g);
    }

    private void drawHeliboy(Graphics g) {
        g.drawImage(heliboy, hb.getEnemyPositionX() - 48,
                hb.getEnemyPositionY() - 48, this);
        g.drawImage(heliboy, hb2.getEnemyPositionX() - 48,
                hb2.getEnemyPositionY() - 48, this);
    }

    private void drawRobot(Graphics g) {
        g.drawImage(robotImageState, robot.getCenterX() - 61,
                robot.getCenterY() - 63, this);
    }

    private void drawBackground(Graphics g) {
        g.drawImage(background, bg1.getBgX(), bg1.getBgY(), this);
        g.drawImage(background, bg2.getBgX(), bg2.getBgY(), this);
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
            if (robot.hasJumped() == false) {
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
}