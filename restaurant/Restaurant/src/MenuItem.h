#ifndef MENUITEM_H
#define MENUITEM_H

#include <iostream>

class MenuItem {
public:
    MenuItem(std::string code = "", std::string name = "", double price = 0);
    //add get methods, etc.

    std::string getCode() const;

    void print() const;

private:
    std::string code; // See sample codes in config.txt
    std::string name; // Full name of the entry
    double price; // price of the item
};

#endif
