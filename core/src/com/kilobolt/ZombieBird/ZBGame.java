package com.kilobolt.ZombieBird;

import com.badlogic.gdx.Game;
import com.kilobolt.Screens.GameScreen;

public class ZBGame extends Game {

    @Override
    public void create() {
        System.out.println("Game Created!");
        setScreen(new GameScreen());
    }

}
