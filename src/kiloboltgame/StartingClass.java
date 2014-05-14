package kiloboltgame;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

public class StartingClass extends Applet implements Runnable, KeyListener {

    @Override
    public void init() {
        setSize(800, 400);
        setBackground(Color.black);
        setFocusable(true);
        addKeyListener(this);
        Frame frame = (Frame) this.getParent().getParent();
        frame.setTitle("Q-Bot Alpha");

    }

    @Override
    public void start() {
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
            repaint();
            try {
                Thread.sleep(17);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void keyPressed(KeyEvent e) {
        getKeyEvent(e);
    }

    @Override
    public void keyReleased(KeyEvent e) {
        getKeyEvent(e);
    }

    private void getKeyEvent(KeyEvent e) {
        switch (e.getKeyCode()) {
        case KeyEvent.VK_UP:
            break;
        case KeyEvent.VK_DOWN:
            break;
        case KeyEvent.VK_LEFT:
            break;
        case KeyEvent.VK_RIGHT:
            break;
        case KeyEvent.VK_SPACE:
            break;
        }
    }

    @Override
    public void keyTyped(KeyEvent e) {
        // TODO Auto-generated method stub

    }
}