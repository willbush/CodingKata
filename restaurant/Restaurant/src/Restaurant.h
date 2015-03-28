#ifndef RESTAURANT_H
#define RESTAURANT_H

#include <fstream>
#include "Table.h"

enum ConfigSection {
    NOT_FOUND, TABLE, WAITER, MENU
};

class Restaurant {

public:
    Restaurant(const std::string&, const std::string&);

    ~Restaurant();

    void run();

    void printTables() const;

    void printWaiters() const;

    void printMenu() const;

private:
    const std::string CONFIG_LOC;
    const std::string ACTIVITY_LOC;

    int tableEntryCount;
    int waiterEntryCount;
    int menuEntryCount;

    ConfigSection configSection;

    Table **tables;
    Waiter **waiters;
    Menu *menu;

    std::fstream configFile;

    void initFromConfig();

    void countInputEntries();

    void initializeObjects();

    void updateSectionAndLine(std::string& line);

    bool lineContains(const std::string&, const std::string&);

    void loadEntriesFromConfig();
};

#endif
