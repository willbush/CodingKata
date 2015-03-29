#include <iostream>
#include "Restaurant.h"

using namespace std;

const string configLocation = "config.txt";
const string activityLocation = "activity.txt";

bool filesExist();

int main() {

    if (filesExist()) {
        Restaurant program(configLocation, activityLocation);
        try {
            program.run();
        } catch (int status) {
            return status;
        }
    } else {
        string notFound = "config.txt or activity.txt not found"
                " in the working directory of the program.";
        cout << notFound << endl;
        return 1;
    }
    return 0;
}

bool filesExist() {
    return ifstream(configLocation.c_str())
            && ifstream(activityLocation.c_str());
}
