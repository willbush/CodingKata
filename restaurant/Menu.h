#ifndef MENU_H
#define MENU_H

#include "MenuItem.h"

class Menu {
public:
    Menu(int items = 100); //constructor to allocate memory
    void addItem(MenuItem item); //add one menu item at a time
//    MenuItem *findItem(string code); //lookup operation
    ~Menu();

    void print() const;

private:
    int maxItems;
    int itemCount;
    MenuItem *items;
};

// Usage:
// Menu menu;
// MenuItem *ip = menu.findItem(code);

// IMPLEMENTATION:
// MenuItem* Menu::findItem(string code) CORRECT
// MenuItem Menu::*findItem(string code) WRONG
#endif
