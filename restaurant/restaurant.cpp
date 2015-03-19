#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include "Table.h"
#include "Tokenizer.h"

using namespace std;

const string CONFIG_LOC = "config.txt";
const string ACTIVITY_LOC = "activity.txt";
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

void freeDynamicMemory();

int main() {
    initFromConfig();

    freeDynamicMemory();
    return 0;
}

void initFromConfig() {
    configFile.open(CONFIG_LOC.c_str(), ios::in);

    if (configFile.fail()) {
        cout << CONFIG_LOC << "not found." << endl;
        exit(1);
    }

    countInputEntries();
    initializeTablesAndWaiters();

    configFile.close();
}

void countInputEntries() {
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

void initializeTablesAndWaiters() {
    foundMenuSection = foundTableSection = foundWaitersSection = false; // reset
    tables = new Table *[tableEntryCount];
    waiters = new Waiter *[waiterEntryCount];
    menu = new Menu(menuEntryCount);
    unsigned int table_i = 0, waiter_i = 0;
    string line;

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        Tokenizer t(line, " ");
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

void updateSectionAndLine(string &line) {
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

bool lineContains(string target, string &line) {
    return line.find(target, 0) != string::npos;
}

bool isInTableSection() {
    return foundTableSection && !foundWaitersSection && !foundMenuSection;
}

bool isInWaiterSection() {
    return foundTableSection && foundWaitersSection && !foundMenuSection;
}

bool isInMenuSection() {
    return foundTableSection && foundWaitersSection && foundMenuSection;
}

void freeDynamicMemory() {
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
