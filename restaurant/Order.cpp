#include "Order.h"

Order::Order(string orderList, Menu *menu) {

}

Order::~Order() {
    delete[] items;
}
