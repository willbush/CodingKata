#include "Order.h"
#include <sstream>

using namespace std;

Order::Order(string entryList, Menu* menu) :
        MAX_ITEMS(entryList.length()) {

    numItems = 0;
    items = new MenuItem*[MAX_ITEMS];

    for (int i = 0; i < MAX_ITEMS; i++)
        items[i] = NULL;

    buildOrder(entryList, menu);
}

Order::~Order() {
//    for (int i = 0; i < MAX_ITEMS; i++)
//        delete items[i];

    delete[] items;
    items = NULL;
}

void Order::buildOrder(const string& entryList, Menu* menu) {
    istringstream ss(entryList);
    string entryCode;
    while (ss >> entryCode)
        items[numItems++] = menu->findItem(entryCode);
}
