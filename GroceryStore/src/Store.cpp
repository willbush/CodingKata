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
const double discountRate = 0.05;
LookupTable<Product *> table;
List<int> *pluList;
fstream inventoryFile;
ofstream outputFile;

void readInventory();
void loadProducts();
void checkout();
double getProductTotal(int);
void writeTableAndFree();
void buildOSS(ostringstream &, int);

int main() {
    pluList = new List<int>();
    table.addRange(0, 9999);
    table.addRange(90000, 99999);

    readInventory();
    checkout();
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

void checkout() {
    string enterPlu = "Enter PLU code or 0 to complete checkout: ";
    int pluCode = 0;
    double total = 0;

    cout << enterPlu;
    cin >> pluCode;

    while (pluCode != 0) {
        if (table[pluCode])
            total += getProductTotal(pluCode);

        cout.precision(2);
        cout << "Total so far $" << fixed << total << endl;
        cout << enterPlu;
        cin >> pluCode;
    }
    double discount = total * discountRate;
    double amountDue = total - discount;

    cout << "Total: $" << total << endl;
    cout << "Discount: $" << discount << endl;
    cout << "Amount due: $" << amountDue << endl;
    cout << "Press y to check out another customer (any other key to quit): ";

    char input;
    cin >> input;
    if (input == 'y')
        checkout();
}

double getProductTotal(int plu) {
    double total = 0;
    double weight = 0;
    Product *item = table[plu];

    if (item->getIsSoldByWeight()) {
        cout << "Enter weight for " << item->getName() << ": ";
        cin >> weight;
        total = weight * item->getPrice();
    } else
        total = item->getPrice();

    return total;
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
