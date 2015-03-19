#ifndef ORDER_H
#define ORDER_H

#include "MenuItem.h"
#include "Menu.h"

class Order {
private:
    int maxItems;  // # of items in the order
    int numItems;  // current # of items in the order
    MenuItem **itemsp;

public:
    //mechanism #1 to setup order object
    Order(int count); //allocates array of pointers to "selected" menu items
    int addItem(MenuItem *itemp); // add one item at a time to the order

    //mechanism #2: order string
//    Order(string orderList, Menu *menu); // alternate way to setup the order

    ~Order();
};

#endif

