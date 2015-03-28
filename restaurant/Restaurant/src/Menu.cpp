#include "Menu.h"

using namespace std;

Menu::Menu(int maxItems) {
    this->maxItems = maxItems;
    items = new MenuItem[maxItems];
    itemCount = 0;
}

Menu::~Menu() {
    delete[] items;
}

void Menu::addItem(MenuItem item) {
    items[itemCount] = item;
    itemCount++;
}

void Menu::print() const {
    cout << "max number of items: " << maxItems << endl;
    cout << "Number of items added: " << itemCount << endl;

    for (int i = 0; i < maxItems; i++) {
        items[i].print();
    }
}

MenuItem *Menu::findItem(string code) {
    MenuItem *item;
    for (int i = 0; i < maxItems; i++)
        if (items[i].getCode() == code)
            *item = items[i];

    return item;
}
