#include "Menu.h"

Menu::Menu(int maxItems) {
    this->maxItems = maxItems;
    items = new MenuItem[maxItems];
    itemCount = 0;
}

void Menu::addItem(MenuItem item) {
    items[itemCount] = item;
    itemCount++;
}

Menu::~Menu() {
    delete[] items;
}

void Menu::print() const {
    cout << "max number of items: " << maxItems << endl;
    cout << "Number of items added: " << itemCount << endl;
    for (int i = 0; i < maxItems; i++) {
        items[i].print();
    }
}
