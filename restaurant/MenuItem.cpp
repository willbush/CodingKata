#include "MenuItem.h"

MenuItem::MenuItem(string code, string name, double price) {
    this->code = code;
    this->name = name;
    this->price = price;
}

void MenuItem::print() const {
    cout << code << " " << name << " " << price << endl;
}
