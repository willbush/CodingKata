#ifndef MENUITEM_H
#define MENUITEM_H

#include <iostream>

class MenuItem {
public:
    MenuItem(std::string code = "", std::string name = "", double price = 0);

    std::string getCode() const;

    double getPrice() const;

    void print() const;

private:
    std::string code;
    std::string name;
    double price;
};

#endif
