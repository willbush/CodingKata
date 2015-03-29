#include "Restaurant.h"
#include <cstdlib>
#include <sstream>

using namespace std;

Restaurant::Restaurant(const string& configLoc, const string& activityLoc)
        : CONFIG_LOC(configLoc), ACTIVITY_LOC(activityLoc) {

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

    for (int i = 0; i < orderCount; i++)
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
    openActivityFile();
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
        int tableID = -1, maxSeats = -1;

        if (configSection == TABLE && input >> tableID >> maxSeats)
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

void Restaurant::openActivityFile() {
    actvityfile.open(ACTIVITY_LOC.c_str(), ios::in);
    try {
        processActivities();
    } catch (const string& exceptionMessage) {
        cout << exceptionMessage << endl;
        exit(EXIT_FAILURE);
    }
    actvityfile.close();
}

void Restaurant::processActivities() {
    string formatError = "The following line in the activity "
            "file is incorrectly formatted:\n";
    string line, entryList;
    char table, command;
    int tableID = -1, partySize = -1;

    while (getline(actvityfile, line)) {
        istringstream input(line);

        input >> table >> tableID >> command;

        if (table != 'T')
            throw formatError + line
                    + "\nExpected table command T. Found command: " + table;

        switch (command) {

        case 'P':
            seatParty(partySize, tableID);
            input >> partySize;
            break;

        case 'O':
            getline(input, entryList); // gets rest of line after 'O'
            placeOrder(entryList, tableID);
            break;

        case 'S':
            serve(tableID);
            break;

        case 'C':
            checkPartyOut(tableID);
            break;

        default:
            throw formatError + line
                    + "\nExpected command P, O, S, or C. Found command: "
                    + command;
        }
    }
}

void Restaurant::seatParty(int partySize, int tableID) {
    tables[tableID - 1]->seatParty(partySize);
}

void Restaurant::placeOrder(const string& entryList, int tableID) {
    orders[orderCount] = new Order(entryList, menu);
    tables[tableID - 1]->partyOrdered(orders[orderCount++]);
}

void Restaurant::serve(int tableID) {
    tables[tableID - 1]->partyServed();
}

void Restaurant::checkPartyOut(int tableID) {
    tables[tableID - 1]->partyCheckout();
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
