#include <fstream>
#include <iostream>
#include "Product.h"
#include "LookupTable.h"

using namespace std;

LookupTable<Product *> table;
const string inventoryLoc = "inventory.csv";
fstream inventoryFile;

void readInventory();
void loadProducts();
void processElement(int, const string&);

int main() {
    table.addRange(0, 9999);
    table.addRange(90000, 99999);
    readInventory();
    return 0;
}

void readInventory() {
    inventoryFile.open(inventoryLoc.c_str(), ios::in);
    loadProducts();
    inventoryFile.close();
}

void loadProducts() {
    string buffer;
    string line = "";
    int element;

    while (getline(inventoryFile, line)) {
        buffer = "";
        element = 0;
        for (string::size_type i = 0; i < line.size(); i++) {
            if (line[i] == ',') {
                processElement(element, buffer);
                element++;
                buffer = "";
            } else
                buffer += line[i];
        }
        processElement(element, buffer);
    }
}

void processElement(int element, const string& buffer) {
    switch (element) {
    case 0:
        cout << "plu is " << buffer << endl;
        break;
    case 1:
        cout << "name is " << buffer << endl;
        break;
    case 2:
        cout << "is sold by weight " << buffer << endl;
        break;
    case 3:
        cout << "price is " << buffer << endl;
        break;
    case 4:
        cout << "inv is " << buffer << endl;
        break;
    }
}
