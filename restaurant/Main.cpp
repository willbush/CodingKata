#include <iostream>
#include "Restaurant.h"

bool filesExist(string const &configLocation, string const &activityLocation);

int main() {
    std::string configLocation = "config.txt";
    std::string activityLocation = "activity.txt";

    if (filesExist(configLocation, activityLocation)) {
        Restaurant program(configLocation, activityLocation);
        program.run();
//        program.printTables();
//        program.printWaiters();
//        program.printMenu();
    } else {
        cout << "config.txt or activity.txt not found in the working directory of Main.cpp\n";
        return 1;
    }

    return 0;
}

bool filesExist(string const &configLocation, string const &activityLocation) {
    return ifstream(configLocation.c_str()) && ifstream(activityLocation.c_str());
}
