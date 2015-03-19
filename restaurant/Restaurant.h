#ifndef RESTAURANT_H
#define RESTAURANT_H

#include <iostream>
#include <fstream>
#include <sstream>
#include "Table.h"

using namespace std;

class Restaurant {

public:
    Restaurant(string const &, string const &);

    ~Restaurant();

    void run();

private:
    const string CONFIG_LOC;
    const string ACTIVITY_LOC;

    unsigned int tableEntryCount = 0;
    unsigned int waiterEntryCount = 0;
    unsigned int menuEntryCount = 0;

    bool foundTableSection = false;
    bool foundWaitersSection = false;
    bool foundMenuSection = false;

    Table **tables;
    Waiter **waiters;
    Menu *menu;

    fstream configFile;

    void initFromConfig();

    void countInputEntries();

    void initializeTablesAndWaiters();

    void updateSectionAndLine(string &line);

    bool lineContains(string const &, string const &);

    bool isInTableSection();

    bool isInWaiterSection();

    bool isInMenuSection();
};

#endif