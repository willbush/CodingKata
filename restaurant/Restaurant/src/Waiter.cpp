#include "Waiter.h"
#include "stdlib.h"

using namespace std;

Waiter::Waiter(const string& name, const string& tableList, Table **tables) {
    int maxPossibleSize = tableList.length();

    myName = name;
    myTables = new Table *[maxPossibleSize];
    tableAssignmentCount = 0;

    for (int i = 0; i < maxPossibleSize; i++) {
        myTables[i] = NULL;
    }

    assignTablesToWaiter(tableList, tables);
}

Waiter::~Waiter() {
    delete[] myTables;
    myTables = NULL;
}

void Waiter::assignTablesToWaiter(const string &tableList, Table **tables) {
    string buffer = "";
    int tableID = -1;

    for (string::size_type i = 0; i < tableList.size(); i++) {
        if (tableList[i] == ',') {
            tableID = atoi(buffer.c_str());
            tables[tableID - 1]->assignWaiter(this);
            myTables[tableAssignmentCount++] = tables[tableID - 1];
            buffer = "";
        } else
            buffer += tableList[i];
    }
    tableID = atoi(buffer.c_str());
    tables[tableID - 1]->assignWaiter(this);
    myTables[tableAssignmentCount++] = tables[tableID - 1];
}

void Waiter::print() const {
    cout << "waiter name: " << myName << endl;
    for (int i = 0; i < tableAssignmentCount; i++) {
        myTables[i]->print();
    }
}
