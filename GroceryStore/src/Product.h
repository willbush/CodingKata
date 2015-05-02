#ifndef PRODUCT_H
#define PRODUCT_H

#include <iostream>
#include <string>

using namespace std;

class Product {
public:
    Product(int code = 0, string name = "", bool isSoldByWeight = true,
            double price = 0.0, double inv = 0.0);
    bool soldByWeight(void);
    double computeCost(double units);
    string getName(void);
    int getPluCode(void);
    double getPrice(void);
    double getInventory(void);

private:
    int pluCode;
    string name;
    double price, inventory;
    bool isSoldByWeight;
};

#endif
