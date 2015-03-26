#include "Waiter.h"

Waiter::Waiter(string name, string TableList, Table *tables) {
    this->name = name;
    tables = NULL;
}

Waiter::~Waiter() {
    delete[] tables;
    tables = NULL;
}

void Waiter::print() const {
    cout << "Waiter name: " << name << endl;
}
