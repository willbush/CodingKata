#include "Waiter.h"
#include "stdlib.h"

using namespace std;

Waiter::Waiter(const string& name, const string& tableList, Table **tables) {
    int maxPossibleSize = tableList.length();

    myName = name;
    myTables = new Table *[maxPossibleSize];
    tableAssignmentCount = 0;

    if (tables != NULL)
        assignWaiterToTables(tableList, tables);
}
/*
 * Each table in the array is deallocated in the Restaurant class.
 */
Waiter::~Waiter() {
    delete[] myTables;
    myTables = NULL;
}

void Waiter::assignWaiterToTables(const string &tableList, Table **tables) {
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

string Waiter::getName() const {
    return myName;
}
