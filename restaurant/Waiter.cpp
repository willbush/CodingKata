#include "Waiter.h"

Waiter::Waiter(string name, string TableList, Table **tables) {
    this->name = name;
    this->tables = tables;
    parseTableList();
}

Waiter::~Waiter() {
    delete[] tables;
    tables = NULL;
}

void Waiter::parseTableList() {

}

void Waiter::print() const {
    cout << "Waiter name: " << name << endl;
}
