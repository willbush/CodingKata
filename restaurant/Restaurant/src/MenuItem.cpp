#include "MenuItem.h"
#include <iostream>

using namespace std;

MenuItem::MenuItem(string code, string name, double price) {
    this->code = code;
    this->name = name;
    this->price = price;
}

void MenuItem::print() const {
    cout << code << " " << name << " " << price << endl;
}

string MenuItem::getCode() const {
    return code;
}

double MenuItem::getPrice() const {
    return price;
}
