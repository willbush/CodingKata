#include "Restaurant.h"

Restaurant::Restaurant(string configLoc, string activityLoc)
        : CONFIG_LOC(configLoc), ACTIVITY_LOC(activityLoc) {
}

void Restaurant::run() {
    initFromConfig();
}

void Restaurant::initFromConfig() {
    configFile.open(CONFIG_LOC.c_str(), ios::in);

    if (configFile.fail()) {
        cout << CONFIG_LOC << "not found." << endl;
        exit(1);
    }

    countInputEntries();
    initializeTablesAndWaiters();

    configFile.close();
}

void Restaurant::countInputEntries() {
    string line;

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableNum = -1, maxSeats = -1;

        if (isInTableSection() && input >> tableNum >> maxSeats)
            tableEntryCount++;

        else if (isInWaiterSection() && line != "")
            waiterEntryCount++;

        else if (isInMenuSection() && line != "")
            menuEntryCount++;
    }
    configFile.clear();
    configFile.seekg(0, ios::beg); // reset file to the top
}

void Restaurant::initializeTablesAndWaiters() {
    foundMenuSection = foundTableSection = foundWaitersSection = false; // reset
    tables = new Table *[tableEntryCount];
    waiters = new Waiter *[waiterEntryCount];
    menu = new Menu(menuEntryCount);
    unsigned int table_i = 0, waiter_i = 0;
    string line;

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableNum = -1, maxSeats = -1;

        if (isInTableSection() && input >> tableNum >> maxSeats) {
            tables[table_i] = new Table(tableNum, maxSeats);
            table_i++;
        }
        else if (isInWaiterSection() && line != "") {
            string name, tableList;
            input >> name >> tableList;
            waiters[waiter_i] = new Waiter(name, tableList, tables[0]);
            waiter_i++;
        }
        else if (isInMenuSection() && line != "") {
            string code, name;
            double price;
            input >> code >> name >> price;
            menu->addItem(MenuItem(code, name, price));
        }
    }
    configFile.clear();
    configFile.seekg(0, ios::beg); // reset file to the top
}

void Restaurant::updateSectionAndLine(string &line) {
    if (!foundTableSection && lineContains("Tables:", line)) {
        foundTableSection = true;
        getline(configFile, line); // change line to next line
    }
    else if (!foundWaitersSection && lineContains("Waiters:", line)) {
        foundWaitersSection = true;
        getline(configFile, line);
    }
    else if (!foundMenuSection && lineContains("Menu:", line)) {
        foundMenuSection = true;
        getline(configFile, line);
    }
}

bool Restaurant::lineContains(string target, string &line) {
    return line.find(target, 0) != string::npos;
}

bool Restaurant::isInTableSection() {
    return foundTableSection && !foundWaitersSection && !foundMenuSection;
}

bool Restaurant::isInWaiterSection() {
    return foundTableSection && foundWaitersSection && !foundMenuSection;
}

bool Restaurant::isInMenuSection() {
    return foundTableSection && foundWaitersSection && foundMenuSection;
}

Restaurant::~Restaurant() {
    for (int i = 0; i < tableEntryCount; i++) {
        delete tables[i];
    }
    for (int i = 0; i < waiterEntryCount; i++) {
        delete waiters[i];
    }
    delete[] waiters;
    delete[] tables;
    delete menu;
}

