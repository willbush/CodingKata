#ifndef WAITER_H
#define WAITER_H

#include "Table.h"

// to take care of circular reference.
class Table;

class Waiter {
public:

    Waiter(const std::string& name = "", const std::string& tableList = "",
            Table **table = NULL);

    ~Waiter();

    void print() const;

private:
    int tableAssignmentCount;
    std::string myName;
    Table **myTables;

    void assignTablesToWaiter(const std::string& tableList, Table **tables);
};

#endif
