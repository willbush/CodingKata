#include <iostream>
#include <fstream>

using namespace std;

const string CONFIG_LOC = "config.txt";
const string ACTIVITY_LOC = "activity.txt";
unsigned int tableEntryCount = 0;
unsigned int waiterEntryCount = 0;
unsigned int menuEntryCount = 0;
fstream configFile;

void countInputEntries();

bool lineContains(string &line, string search);

void openConfig();

int main() {
    openConfig();
    return 0;
}

void openConfig() {
    configFile.open(CONFIG_LOC, ios::in);

    if (configFile.fail()) {
        cout << CONFIG_LOC << "file not found." << endl;
        exit(1);
    }

    countInputEntries();
    configFile.close();
}

void countInputEntries() {
    unsigned int lineCount = 0;
    string line;

    while (getline(configFile, line)) {
        lineCount++;

        if (lineContains(line, "Waiters"))
            tableEntryCount = lineCount;

        else if (lineContains(line, "Menu"))
            waiterEntryCount = lineCount - tableEntryCount;
    }
    menuEntryCount = lineCount - tableEntryCount - waiterEntryCount;
}

bool lineContains(string &line, string search) {
    return line.find(search, 0) != string::npos;
}