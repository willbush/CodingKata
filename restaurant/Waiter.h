#ifndef WAITER_H
#define WAITER_H

#include <string>
#include "Table.h"

class Table; // to take care of circular reference.

class Waiter {
public:
    ~Waiter();

    Waiter(string name = "", string TableList = "", Table *tables = NULL);
    // waiter's name, his table list as a string, table array pointer

    void print() const;

private:
    string name;      // waiter's name
    int numTables;    // number of tables waiter is in-charge for
    Table *tables;    // waiter's table list
};

#endif
