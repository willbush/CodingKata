#ifndef TABLE_H
#define TABLE_H

#include "Order.h"
#include "Waiter.h"

enum TableStatus {
    IDLE, SEATED, ORDERED, SERVED
};

/* equivalent in C:
#define IDLE 0
#define SEATED 1
...

equivalent in C++:
const int IDLE = 0;
const int SEATED = 1;
...
*/

class Waiter; // to take care of circular reference.

class Table {
private:
    int tableId;        // table number
    const int maxSeats;    // table seat capacity
    TableStatus status;    // current status, you can use assign like
    // status = IDLE;
    int numPeople;        // number of people in current party
    Order *order;        // current party's order
    Waiter *waiter;        // pointer to waiter for this table

public:
    Table(int tblid = 0, int mseats = 0);    // initialization, IDLE
    void assignWaiter(Waiter *person);    // initially no waiter
    void partySeated(int npeople);        // process IDLE --> SEATED
    void partyOrdered(Order *order);    // process SEATED --> ORDERED
    void partyServed(void);            // process ORDERED --> SERVED
    void partyCheckout(void);        // process SERVED --> IDLE
};

#endif
