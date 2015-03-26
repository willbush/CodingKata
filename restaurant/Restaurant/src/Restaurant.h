#ifndef RESTAURANT_H
#define RESTAURANT_H

#include <iostream>
#include <fstream>
#include <sstream>
#include "Table.h"

enum ConfigSection {
    NOT_FOUND, TABLE, WAITER, MENU
};

class Restaurant {

public:
    Restaurant(string const &, string const &);

    ~Restaurant();

    void run();

    void printTables() const;

    void printWaiters() const;

    void printMenu() const;

private:
    const string CONFIG_LOC;
    const string ACTIVITY_LOC;

    unsigned int tableEntryCount;
    unsigned int waiterEntryCount;
    unsigned int menuEntryCount;

    ConfigSection configLocation;

    Table **tables;
    Waiter **waiters;
    Menu *menu;

    fstream configFile;

    void initFromConfig();

    void countInputEntries();

    void initializeObjects();

    void updateSectionAndLine(string &line);

    bool lineContains(string const &, string const &);

    void loadEntriesFromConfig();
};

#endif
