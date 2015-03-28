#include "Restaurant.h"
#include <sstream>

using namespace std;

Restaurant::Restaurant(const string& configLoc, const string& activityLoc) :
        CONFIG_LOC(configLoc), ACTIVITY_LOC(activityLoc) {

    tableEntryCount = waiterEntryCount = menuEntryCount = orderCount = 0;
    configSection = NOT_FOUND;

    tables = NULL;
    waiters = NULL;
    orders = NULL;
    menu = NULL;
}

Restaurant::~Restaurant() {
    for (int i = 0; i < tableEntryCount; i++)
        delete tables[i];

    for (int i = 0; i < waiterEntryCount; i++)
        delete waiters[i];

    for (int i = 0; i < menuEntryCount; i++)
        delete orders[i];

    delete[] waiters;
    delete[] tables;
    delete[] orders;
    delete menu;

    waiters = NULL;
    tables = NULL;
    orders = NULL;
    menu = NULL;
}

void Restaurant::run() {
    initFromConfig();
    processActivities();
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
    orders = new Order *[menuEntryCount];

    for (int i = 0; i < tableEntryCount; i++)
        tables[i] = NULL;

    for (int i = 0; i < waiterEntryCount; i++)
        waiters[i] = NULL;

    for (int i = 0; i < menuEntryCount; i++)
        orders[i] = NULL;

    menu = new Menu(menuEntryCount);
}

void Restaurant::loadEntriesFromConfig() {
    unsigned int table_i = 0, waiter_i = 0;
    string line = "";

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableNum = -1, maxSeats = -1;

        if (configSection == TABLE && input >> tableNum >> maxSeats)
            tables[table_i++] = new Table(tableNum, maxSeats);
        else if (configSection == WAITER && line != "") {
            string name = "", tableList = "";
            input >> name >> tableList;
            waiters[waiter_i++] = new Waiter(name, tableList, tables);
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

void Restaurant::updateSectionAndLine(string& line) {
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

bool Restaurant::lineContains(const string& target, const string& line) {
    return line.find(target, 0) != string::npos;
}

void Restaurant::processActivities() {
    actvityfile.open(ACTIVITY_LOC.c_str(), ios::in);
    string line, entryList;
    char table, command;
    int tableID, partySize;

    while (getline(actvityfile, line)) {
        istringstream ss(line);

        ss >> table >> tableID >> command;

        switch (command) {

        case 'P':
            seatParty(partySize, tableID);
            ss >> partySize;
            break;

        case 'O':
            getline(ss, entryList);
            placeOrder(entryList, tableID);
            break;

        case 'S':
            serve(tableID);
            break;

        case 'C':
            giveCheck(tableID);
            break;
        }
    }
    actvityfile.close();
}

void Restaurant::seatParty(int partySize, int tableID) {
    tables[tableID - 1]->seatParty(partySize);
}

void Restaurant::placeOrder(const string& entryList, int tableID) {
    orders[orderCount] = new Order(entryList, menu);
    tables[tableID - 1]->partyOrdered(orders[orderCount++]);
}

void Restaurant::serve(int tableID) {

}

void Restaurant::giveCheck(int tableID) {

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
