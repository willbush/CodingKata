#include <iostream>
#include "Restaurant.h"

int main() {
    std::string configLocation = "config.txt";
    std::string activityLocation = "activity.txt";

    Restaurant program(configLocation, activityLocation);
    program.run();

    return 0;
}
