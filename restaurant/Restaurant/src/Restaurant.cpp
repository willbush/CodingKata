#include "Restaurant.h"
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
}

void Restaurant::run() {
    initFromConfig();
    openActivityFile();
}

void Restaurant::initFromConfig() {
    configFile.open(CONFIG_LOC.c_str(), ios::in);
    countInputEntries();
    initializeObjects();
    tryLoadEntriesFromConfig();
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
    menu = new Menu(menuEntryCount);
}

void Restaurant::tryLoadEntriesFromConfig() {
    try {
        loadEntriesFromConfig();
    } catch (const char* exceptionMessage) {
        cout << exceptionMessage << endl;
        throw 1; // let main catch and stack unwind.
    }
}

void Restaurant::loadEntriesFromConfig() {
    unsigned int table_i = 0, waiter_i = 0;
    string line = "";

    while (getline(configFile, line)) {
        updateSectionAndLine(line);

        istringstream input(line);
        int tableID = -1, maxSeats = -1;

        if (configSection == TABLE && input >> tableID >> maxSeats) {
            checkTableID(tableID, table_i);
            checkMaxSeats(maxSeats);
            tables[table_i++] = new Table(tableID, maxSeats);
        } else if (configSection == WAITER && line != "") {
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

void Restaurant::checkTableID(int tableID, int table_i) const {
    if (tableID != (table_i + 1)) {
        throw "The configuration file is incorrectly formatted:\n"
                "The tableID must be greater than 0 and sorted in increasing order\n"
                "from the top of the Table section to the end of it.";
    }
}

void Restaurant::checkMaxSeats(int maxSeats) const {
    if (maxSeats < 0) {
        throw "The configuration file is incorrectly formatted:\n"
                "max seats cannot be negative";
    }
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
    activityFile.open(ACTIVITY_LOC.c_str(), ios::in);
    tryProcessActiviies();
    activityFile.close();
}

void Restaurant::tryProcessActiviies() {
    try {
        processActivities();
    } catch (const string& exceptionMessage) {
        cout << exceptionMessage << endl;
        throw 1; // let main catch and stack unwind.
    }
}

void Restaurant::processActivities() {
    string formatError = "The following line in the activity "
            "file is incorrectly formatted:\n";
    string line, entryList;
    char table, command;
    int tableID = -1, partySize = -1;

    while (getline(activityFile, line)) {
        istringstream input(line);

        input >> table >> tableID >> command;

        if (table != 'T')
            throw formatError + line
                    + "\nExpected table command T. Found command: " + table;
        if (tableID < 1)
            throw formatError + line + "\nTableID must be greater than 0.";

        switch (command) {

        case 'P':
            input >> partySize;
            seatParty(partySize, tableID);
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
