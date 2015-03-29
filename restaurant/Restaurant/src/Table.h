#ifndef TABLE_H
#define TABLE_H

#include "Order.h"
#include "Waiter.h"

enum TableStatus {
    UNASSIGNED, IDLE, SEATED, ORDERED, SERVED
};

// to take care of circular reference.
class Waiter;

class Table {
public:

    Table(int tableID = 0, int maxSeats = 0); // initialization, UNASSGINED

    ~Table();

    void assignWaiter(Waiter *person); // UNASSIGNED --> IDLE

    void seatParty(int numOfPeople); // IDLE --> SEATED

    void partyOrdered(Order *order);  // SEATED --> ORDERED

    void partyServed(); // ORDERED --> SERVED

    void partyCheckout(); // SERVED --> IDLE

private:
    TableStatus status;
    Order *order;
    Waiter *waiter;
    const int MAX_SEATS;
    int tableID;
    int numPeople;

    bool partySizeIsValid(int numOfPeople);

    void displayCheck() const;
};

#endif
