#include "Order.h"

Order::Order(int count) {

}

int Order::addItem(MenuItem *itemp) {
    return 0;
}

Order::~Order() {
    delete [] itemsp;
}
