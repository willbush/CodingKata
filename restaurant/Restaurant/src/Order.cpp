#include "Order.h"

Order::Order(string orderList, Menu *menu) {
    numItems = 0;
    items = NULL;
}

Order::~Order() {
    delete[] items;
}
