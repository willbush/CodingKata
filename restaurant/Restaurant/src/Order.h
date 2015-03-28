#ifndef ORDER_H
#define ORDER_H

#include "MenuItem.h"
#include "Menu.h"

class Order {
private:
    // const int MAX_ITEMS;  // # of items in the order
    int numItems;  // current # of items in the order
    MenuItem **items;

public:
    // mechanism #1 to setup order object
    // Order(int count); //allocates array of pointers to "selected" menu items
    // int addItem(MenuItem *item); // add one item at a time to the order

    //mechanism #2: order string
    Order(std::string orderList, Menu *menu); // alternate way to setup the order

    ~Order();
};

#endif

