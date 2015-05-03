#include <fstream>
#include <iostream>
#include "stdlib.h"
#include "Product.h"
#include "LookupTable.h"
#include "List.h"

using namespace std;

LookupTable<Product *> table;
List<int> *pluList;
const string inventoryLoc = "inventory.csv";
fstream inventoryFile;

void readInventory();
void loadProducts();
void writeTable();
void freeMemory();

int main() {
    pluList = new List<int>();
    table.addRange(0, 9999);
    table.addRange(90000, 99999);
    readInventory();
    writeTable();
    freeMemory();
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
        element = 0; // line element number

        // parameters for the Product object
        int pluCode = -1;
        string name = "";
        bool isSoldByWeight = false;
        double price = 0.0, inventory = 0.0;

        for (string::size_type i = 0; i < line.size(); i++) {
            if (line[i] == ',') {
                switch (element) {
                case 0:
                    pluCode = atoi(buffer.c_str());
                    break;
                case 1:
                    name = buffer;
                    break;
                case 2:
                    isSoldByWeight = atoi(buffer.c_str());
                    break;
                case 3:
                    price = atof(buffer.c_str());
                    break;
                }
                element++;
                buffer = "";
            } else
                buffer += line[i];
        }
        inventory = atof(buffer.c_str()); // last element is the inventory number

        table[pluCode] = new Product(pluCode, name, isSoldByWeight, price,
                inventory);
        pluList->add(pluCode);
    }

}

void writeTable() {
}

void freeMemory() {
    while (pluList->hasNext()) {
        int pluCode = pluList->removeHead();
        cout << pluCode << endl;
        delete table[pluCode];
    }
    delete pluList;
}
