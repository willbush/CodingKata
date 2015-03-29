#include "Menu.h"

using namespace std;

Menu::Menu(int maxItems) : MAX_ITEMS(maxItems) {
    items = new MenuItem[MAX_ITEMS];
    itemCount = 0;
}

Menu::~Menu() {
    delete[] items;
}

void Menu::addItem(MenuItem item) {
    items[itemCount++] = item;
}

MenuItem* Menu::findItem(const string& code) const {
    for (int i = 0; i < MAX_ITEMS; i++)
        if (items[i].getCode() == code)
            return &items[i];

    return NULL;
}
