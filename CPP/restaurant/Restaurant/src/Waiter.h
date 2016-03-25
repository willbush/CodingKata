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

    std::string getName() const;

private:
    int tableAssignmentCount;
    std::string myName;
    Table **myTables;
    void assignWaiterToTables(const std::string& tableList, Table **tables);
};

#endif
