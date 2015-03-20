#ifndef MENUITEM_H
#define MENUITEM_H

#include <iostream>
#include <string>

using namespace std;

class MenuItem {
public:
    MenuItem(string code = "", string name = "", double price = 0);
    //add get methods, etc.

    void print() const;

private:
    string code; // See sample codes in config.txt
    string name; // Full name of the entry
    double price; // price of the item

};

#endif
