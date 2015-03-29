#ifndef MENU_H
#define MENU_H

#include "MenuItem.h"

class Menu {
public:
    Menu(int maxItems = 100);

    ~Menu();

    void addItem(MenuItem item);

    MenuItem* findItem(const std::string& code) const;

private:
    const int MAX_ITEMS;
    int itemCount;
    MenuItem *items;
};
#endif
