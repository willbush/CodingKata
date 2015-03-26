#include "Waiter.h"

Waiter::Waiter(string name, string TableList, Table *tables) {
    this->name = name;
    this->tables = NULL;
    numTables = 0;
}

Waiter::~Waiter() {
    delete[] tables;
    tables = NULL;
}

void Waiter::print() const {
    cout << "Waiter name: " << name << endl;
}
