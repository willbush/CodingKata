#ifndef ORDER_H
#define ORDER_H

#include "Menu.h"

class Order {

public:
    Order(std::string orderList = "", Menu* menu = NULL);

    ~Order();

    double getTotal() const;

    void print() const;

private:
    int numItems;
    const unsigned int MAX_ITEMS;
    double total;
    MenuItem **items;

    void buildOrder(const std::string& entryList, Menu* menu);
};

#endif

