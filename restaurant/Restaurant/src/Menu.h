#ifndef MENU_H
#define MENU_H

#include "MenuItem.h"

class Menu {
public:
    Menu(int maxItems = 100); //constructor to allocate memory
    void addItem(MenuItem item); //add one menu item at a time
    MenuItem* findItem(const std::string& code) const;
    ~Menu();

    void print() const;

private:
    const int MAX_ITEMS;
    int itemCount;
    MenuItem *items;
};
#endif
