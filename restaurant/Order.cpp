#include "Order.h"

Order::Order(int count) {

}

int Order::addItem(MenuItem *item) {
    return 0;
}

Order::~Order() {
    delete [] items;
}
