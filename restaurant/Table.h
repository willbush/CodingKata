#ifndef TABLE_H
#define TABLE_H

#include "Order.h"
#include "Waiter.h"

enum TableStatus {
    IDLE, SEATED, ORDERED, SERVED
};

class Waiter; // to take care of circular reference.

class Table {
private:
    int tableId;
    const int maxSeats;
    TableStatus status;
    int numPeople;
    Order *order;
    Waiter *waiter;

public:
    Table(int tblid = 0, int mseats = 0); // initialization, IDLE
    void assignWaiter(Waiter *person); // initially no waiter
    void partySeated(int numOfPeople); // process IDLE --> SEATED
    void partyOrdered(Order *order);  // process SEATED --> ORDERED
    void partyServed(); // process ORDERED --> SERVED
    void partyCheckout(); // process SERVED --> IDLE
    void print();
    ~Table();
};

#endif
