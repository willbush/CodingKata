#ifndef RESTAURANT_H
#define RESTAURANT_H


class Restaurant {

public:
    Restaurant(string, string);

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

    void countInputEntries();

    bool lineContains(string, string &);

    bool isInTableSection();

    bool isInWaiterSection();

    bool isInMenuSection();

    void initFromConfig();

    void initializeTablesAndWaiters();

    void updateSectionAndLine(string &line);
};

#endif