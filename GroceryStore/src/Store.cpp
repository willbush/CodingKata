#include <fstream>
#include <iostream>
#include "Product.h"
#include "LookupTable.h"

using namespace std;

LookupTable<Product *> table;
string inventoryLoc = "inventory.csv";
fstream inventoryFile;

void readInventory();
void loadProducts();

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
    string line = "";
    string buffer;

    while (getline(inventoryFile, line)) {
        buffer = "";
        for (string::size_type i = 0; i < line.size(); i++) {
            if (line[i] == ',') {
                cout << buffer << " ";
                buffer = "";
            } else
                buffer += line[i];
        }
        cout << buffer << " " << endl;
    }
}
