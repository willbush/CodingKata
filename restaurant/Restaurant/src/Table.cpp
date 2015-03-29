#include "Table.h"

using namespace std;

Table::Table(int tableID, int maxSeats)
        : MAX_SEATS(maxSeats) {
    this->tableID = tableID;
    numPeople = 0;
    status = NO_WAITER;
    waiter = NULL;
    order = NULL;
}

/*
 * The deletion of waiter and order pointers is not required here
 * because it is handled in the Restaurant class.
 */
Table::~Table() {
    order = NULL;
    waiter = NULL;
}

void Table::assignWaiter(Waiter *waiter) {
    cout << "try to assign waiter: " << endl;
    cout << " at table " << tableID << endl;
    waiter->print();
    if (status == NO_WAITER) {
        this->waiter = waiter;
        status = IDLE;
        cout << "waiter assigned" << endl;
    }
}

void Table::seatParty(int numOfPeople) {
    cout << "try to seat " << numOfPeople << endl;
    cout << " at table " << tableID << endl;
    if (status == IDLE && numOfPeople <= MAX_SEATS) {
        cout << "party seated" << endl;
        this->numPeople = numOfPeople;
        status = SEATED;
    }
}

void Table::partyOrdered(Order *order) {
    cout << "try to order..." << endl;
    cout << " at table " << tableID << endl;
    if (status == SEATED) {
        this->order = order;
        status = ORDERED;
    }
}

void Table::partyServed() {
    cout << "try to serve..." << endl;
    cout << " at table " << tableID << endl;
    if (status == ORDERED) {
        cout << "table served." << endl;
        status = SERVED;
    }
}

void Table::partyCheckout() {
    cout << "try to check out..." << endl;
    cout << " at table " << tableID << endl;
    if (status == SERVED) {
        status = IDLE;
        cout << "Order total: " << order->getTotal() << endl;
    }
}

void Table::print() const {
    cout << "table ID: " << tableID << " " << "max seats: " << MAX_SEATS
            << endl;
}

TableStatus Table::getStatus() {
    return status;
}

int Table::getPartySize() {
    return numPeople;
}
