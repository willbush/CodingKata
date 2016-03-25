#include "MenuItem.h"

using namespace std;

MenuItem::MenuItem(string code, string name, double price) {
    this->code = code;
    this->name = name;
    this->price = price;
}

string MenuItem::getCode() const {
    return code;
}

double MenuItem::getPrice() const {
    return price;
}

void MenuItem::print() const {
    cout.precision(2);
    cout << code << " " << name << " " << fixed << price << endl;
}
