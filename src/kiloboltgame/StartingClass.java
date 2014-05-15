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
    private Image image, robotImage;
    private Graphics second;
    private URL base;

    @Override
    public void init() {
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

        robotImage = getImage(base, "data/character.png");
    }

    @Override
    public void start() {
        robot = new Robot();
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
            repaint();
            try {
                Thread.sleep(17);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
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
        g.drawImage(robotImage, robot.getCenterX() - 61,
                robot.getCenterY() - 63, this);
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
            System.out.println("move down");
            break;
        case KeyEvent.VK_LEFT:
        case KeyEvent.VK_A:
            robot.moveLeft();
            break;
        case KeyEvent.VK_RIGHT:
        case KeyEvent.VK_D:
            robot.moveRight();
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
            System.out.println("Stop moving down");
            break;
        case KeyEvent.VK_LEFT:
        case KeyEvent.VK_A:
            robot.stop();
            break;
        case KeyEvent.VK_RIGHT:
        case KeyEvent.VK_D:
            robot.stop();
            break;
        case KeyEvent.VK_SPACE:
            System.out.println("Stop jumping");
            break;
        }
    }

    @Override
    public void keyTyped(KeyEvent e) {
        // TODO Auto-generated method stub

    }
}