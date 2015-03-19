#include "Waiter.h"

Waiter::Waiter(string name, string TableList, Table *table) {
    this->name = name;
}

Waiter::~Waiter() {
    delete[] tables;
}
