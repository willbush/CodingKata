#include "Menu.h"

Menu::Menu(int numItems) {
    this->numItems = numItems;
}

void Menu::addItem(MenuItem item) {

}

Menu::~Menu() {
    delete[] menup;
}
