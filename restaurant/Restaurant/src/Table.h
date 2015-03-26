#ifndef TABLE_H
#define TABLE_H

#include "Order.h"
#include "Waiter.h"

/*
 * An idle table is one that has no waiter. A ready table is one that
 * has a waiter and is ready for a party to be seated.
 */
enum TableStatus {
    IDLE, READY, SEATED, ORDERED, SERVED
};

class Waiter;
// to take care of circular reference.

class Table {
public:

    Table(int tableID = 0, int maxSeats = 0); // initialization, IDLE
    TableStatus getStatus();

    int getPartySize();

    void assignWaiter(Waiter *person); // initially no waiter
    void seatParty(int numOfPeople); // process IDLE --> SEATED
    void partyOrdered(Order *order);  // process SEATED --> ORDERED
    void partyServed(); // process ORDERED --> SERVED
    void partyCheckout(); // process SERVED --> IDLE
    void print() const;

    ~Table();

private:
    TableStatus status;
    Order *order;
    Waiter *waiter;
    const int MAX_SEATS;
    int tableId;
    int numPeople;
};

#endif
