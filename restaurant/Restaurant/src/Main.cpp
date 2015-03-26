#include <iostream>
#include "Restaurant.h"

bool filesExist(string const &configLocation, string const &activityLocation);

int main() {
    std::string configLocation = "config.txt";
    std::string activityLocation = "activity.txt";

    if (filesExist(configLocation, activityLocation)) {
        Restaurant program(configLocation, activityLocation);
        program.run();
        program.printTables();
        program.printWaiters();
        program.printMenu();
    } else {
        string notFound = "config.txt or activity.txt not found"
                " in the working directory of the program.";
        cout << notFound << endl;
        return 1;
    }

    return 0;
}

bool filesExist(string const &configLocation, string const &activityLocation) {
    return ifstream(configLocation.c_str())
            && ifstream(activityLocation.c_str());
}
