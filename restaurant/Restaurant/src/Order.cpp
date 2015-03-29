#include "Order.h"
#include <sstream>

using namespace std;

Order::Order(string entryList, Menu* menu)
        : MAX_ITEMS(entryList.length()) {

    numItems = 0;
    total = 0;
    items = new MenuItem*[MAX_ITEMS];

    if (menu != NULL)
        buildOrder(entryList, menu);
}

/*
 * The deallocation of each MenuItem in items is handled in
 * the Restaurant class.
 */
Order::~Order() {
    delete[] items;
}

void Order::buildOrder(const string& entryList, Menu* menu) {
    istringstream input(entryList);
    string entryCode;

    while (input >> entryCode) {
        items[numItems] = menu->findItem(entryCode);
        total += items[numItems]->getPrice();
        numItems++;
    }
}

double Order::getTotal() const {
    return total;
}

void Order::print() const {
    for (int i = 0; i < numItems; i++)
        items[i]->print();
}
