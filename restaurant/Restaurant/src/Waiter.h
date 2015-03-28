#ifndef WAITER_H
#define WAITER_H

#include "Table.h"

// to take care of circular reference.
class Table;

class Waiter {
public:

    Waiter(std::string name = "", std::string tableList = "",
            Table **table = NULL);

    ~Waiter();

    void print() const;

private:
    int tableAssignmentCount;
    std::string myName;
    Table **myTables;
    int listMaxLength;

    void assignTablesToWaiter(const std::string& tableList, Table **tables);
};

#endif
