#include <fstream>
#include <iostream>
#include <sstream>
#include <iostream>
#include "stdlib.h"
#include "Product.h"
#include "LookupTable.h"
#include "List.h"

using namespace std;

const string inventoryLoc = "inventory.csv";
const string outputName = "output.csv";
LookupTable<Product *> table;
List<int> *pluList;
fstream inventoryFile;
ofstream outputFile;

void readInventory();
void loadProducts();
void writeTableAndFree();
void buildOSS(ostringstream &, int);

int main() {
    pluList = new List<int>();
    table.addRange(0, 9999);
    table.addRange(90000, 99999);

    readInventory();
    writeTableAndFree();
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

void writeTableAndFree() {
    ostringstream oss;
    outputFile.open(outputName.c_str());

    while (pluList->hasNext()) {
        int pluCode = pluList->removeHead();
        buildOSS(oss, pluCode);
        outputFile << oss.str();
        delete table[pluCode];
    }
    delete pluList;
    outputFile.close();
}

void buildOSS(ostringstream& oss, int pluCode) {
    oss << pluCode << "," << table[pluCode]->getName();
    oss << "," << table[pluCode]->getIsSoldByWeight();
    oss << "," << table[pluCode]->getPrice();
    oss << "," << table[pluCode]->getInventory() << endl;
}
