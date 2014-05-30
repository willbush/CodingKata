package com.kilobolt.GameWorld;

import com.kilobolt.GameObjects.Bird;
import com.kilobolt.GameObjects.ScrollHandler;
import com.kilobolt.ZBHelpers.AssetLoader;

public class GameWorld {
    private final Bird bird;
    private final ScrollHandler scroller;
    private boolean isAlive = true;

    public GameWorld(int midPointY) {
        bird = new Bird(33, midPointY - 5, 17, 12);
        final int distanceBelowMidPoint = 66 + midPointY;
        scroller = new ScrollHandler(distanceBelowMidPoint);
    }

    public void update(float delta) {
        bird.update(delta);
        scroller.update(delta);
        if (isAlive && scroller.collides(bird)) {
            scroller.stop();
            AssetLoader.dead.play();
            isAlive = false;
        }
    }

    public Bird getBird() {
        return bird;
    }

    public ScrollHandler getScroller() {
        return scroller;
    }
}
