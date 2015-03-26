#include "Restaurant.h"
#include "Tokenizer.h"

using namespace std;

Restaurant::Restaurant(const string &configLoc, const string &activityLoc) :
        CONFIG_LOC(configLoc), ACTIVITY_LOC(activityLoc) {
    tableEntryCount = waiterEntryCount = menuEntryCount = 0;
    configSection = NOT_FOUND;
    tables = NULL;
    waiters = NULL;
    menu = NULL;
}

void Restaurant::run() {
    initFromConfig();
}

void Restaurant::initFromConfig() {
    configFile.open(CONFIG_LOC.c_str(), ios::in);
    countInputEntries();
    initializeObjects();
    loadEntriesFromConfig();
    configFile.close();
}

void Restaurant::countInputEntries() {
    string line = "";

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableNum = -1, maxSeats = -1;

        if (configSection == TABLE && input >> tableNum >> maxSeats)
            tableEntryCount++;

        else if (configSection == WAITER && line != "")
            waiterEntryCount++;

        else if (configSection == MENU && line != "")
            menuEntryCount++;
    }
    configFile.clear();
    configFile.seekg(0, ios::beg); // reset to the top of the file
}

void Restaurant::initializeObjects() {
    tables = new Table *[tableEntryCount];
    waiters = new Waiter *[waiterEntryCount];
    menu = new Menu(menuEntryCount);
}

void Restaurant::loadEntriesFromConfig() {
    unsigned int table_i = 0, waiter_i = 0;
    string line = "";

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableNum = -1, maxSeats = -1;

        if (configSection == TABLE && input >> tableNum >> maxSeats) {
            tables[table_i] = new Table(tableNum, maxSeats);
            table_i++;
        } else if (configSection == WAITER && line != "") {
            string name = "", tableList = "";
            input >> name >> tableList;
            parseTableList(tableList);
            waiters[waiter_i] = new Waiter(name, tableList, *tables);
            waiter_i++;
        } else if (configSection == MENU && line != "") {
            string code = "", name = "";
            double price = 0;
            input >> code >> name >> price;
            menu->addItem(MenuItem(code, name, price));
        }
    }
    configFile.clear();
    configFile.seekg(0, ios::beg); // reset to the top of the file
}

void Restaurant::parseTableList(const string &tableList) {
    cout << tableList << endl;
    Tokenizer t(tableList, ",");
    string token = "";
    do {
        token = t.next();
        cout << token << endl;
    } while (token != "");
}

void Restaurant::updateSectionAndLine(string &line) {
    if (configSection != TABLE && lineContains("Tables:", line)) {
        configSection = TABLE;
        getline(configFile, line); // change line to next line
    } else if (configSection != WAITER && lineContains("Waiters:", line)) {
        configSection = WAITER;
        getline(configFile, line);
    } else if (configSection != MENU && lineContains("Menu:", line)) {
        configSection = MENU;
        getline(configFile, line);
    }
}

bool Restaurant::lineContains(const string &target, const string &line) {
    return line.find(target, 0) != string::npos;
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
    waiters = NULL;
    tables = NULL;
    menu = NULL;
}

void Restaurant::printTables() const {
    for (int i = 0; i < tableEntryCount; i++)
        tables[i]->print();
}

void Restaurant::printWaiters() const {
    for (int i = 0; i < waiterEntryCount; i++)
        waiters[i]->print();
}

void Restaurant::printMenu() const {
    menu->print();
}
