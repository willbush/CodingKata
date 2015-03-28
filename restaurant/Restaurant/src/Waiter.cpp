#include "Waiter.h"
#include "stdlib.h"

using namespace std;

Waiter::Waiter(const string& name, const string& tableList, Table **tables) {
    myName = name;
    tableAssignmentCount = 0;
    myTables = new Table *[tableList.length()];

    assignTablesToWaiter(tableList, tables);
}

Waiter::~Waiter() {
    // deleting each pointer in the array
    // is not needed since Restaurant class will handle that
    delete[] myTables;
    myTables = NULL;
}

void Waiter::assignTablesToWaiter(const string &tableList, Table **tables) {
    string buffer = "";
    int tableID = -1;

    for (string::size_type i = 0; i < tableList.size(); i++) {
        if (tableList[i] == ',') {
            tableID = atoi(buffer.c_str());
            myTables[tableAssignmentCount++] = tables[tableID - 1];
            buffer = "";
        } else
            buffer += tableList[i];
    }
    tableID = atoi(buffer.c_str());
    myTables[tableAssignmentCount++] = tables[tableID - 1];
}

void Waiter::print() const {
    cout << "Waiter name: " << myName << endl;
    for (int i = 0; i < tableAssignmentCount; i++)
        myTables[i]->print();
}
