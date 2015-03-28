#ifndef MENU_H
#define MENU_H

#include "MenuItem.h"

class Menu {
public:
    Menu(int items = 100); //constructor to allocate memory
    void addItem(MenuItem item); //add one menu item at a time
    MenuItem *findItem(std::string code);
    ~Menu();

    void print() const;

private:
    int maxItems;
    int itemCount;
    MenuItem *items;
};
#endif
