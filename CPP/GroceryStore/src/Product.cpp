#include "Product.h"

Product::Product(int pluCode, string name, bool isSoldByWeight, double price,
        double inventory) {
    this->pluCode = pluCode;
    this->name = name;
    this->isSoldByWeight = isSoldByWeight;
    this->price = price;
    this->inventory = inventory;
}

bool Product::soldByWeight() {
    return isSoldByWeight;
}

double Product::computeCost(double units) {
    inventory -= units;
    return units * price;
}

int Product::getPluCode() {
    return pluCode;
}

string Product::getName() {
    return name;
}

bool Product::getIsSoldByWeight() {
    return isSoldByWeight;
}

double Product::getPrice() {
    return price;
}

double Product::getInventory() {
    return inventory;
}
