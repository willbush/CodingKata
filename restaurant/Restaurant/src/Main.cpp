#include <iostream>
#include "Restaurant.h"

using namespace std;

bool filesExist(const string& configLocation, const string& activityLocation);

int main() {
    string configLocation = "config.txt";
    string activityLocation = "activity.txt";

    if (filesExist(configLocation, activityLocation)) {
        Restaurant program(configLocation, activityLocation);
        program.run();
//        program.printTables();
//        program.printWaiters();
//        program.printMenu();
    } else {
        string notFound = "config.txt or activity.txt not found"
                " in the working directory of the program.";
        cout << notFound << endl;
        return 1;
    }
    return 0;
}

bool filesExist(const string& configLocation, const string& activityLocation) {
    return ifstream(configLocation.c_str())
            && ifstream(activityLocation.c_str());
}
