#include "Table.h"

using namespace std;

Table::Table(int tableID, int maxSeats)
        : MAX_SEATS(maxSeats) {

    this->tableID = tableID;
    numPeople = 0;
    status = UNASSIGNED;
    waiter = NULL;
    order = NULL;
}

/*
 * The deallocation of waiter and order objects is not required here
 * because it is handled in the Restaurant class.
 */
Table::~Table() {
    order = NULL;
    waiter = NULL;
}

void Table::assignWaiter(Waiter *waiter) {
    if (status == UNASSIGNED) {
        this->waiter = waiter;
        status = IDLE;
    }
}

void Table::seatParty(int numOfPeople) {
    if (status == IDLE && partySizeIsValid(numOfPeople)) {
        this->numPeople = numOfPeople;
        status = SEATED;
    }
}

bool Table::partySizeIsValid(int numOfPeople) {
    return numOfPeople > 0 && numOfPeople <= MAX_SEATS;
}

void Table::partyOrdered(Order *order) {
    if (status == SEATED) {
        this->order = order;
        status = ORDERED;
    }
}

void Table::partyServed() {
    if (status == ORDERED) {
        status = SERVED;
    }
}

void Table::partyCheckout() {
    if (status == SERVED) {
        status = IDLE;
        displayCheck();
    }
}

void Table::displayCheck() const {
    cout.precision(2);
    cout << "(¯`·._.· CHECK ·._.·´¯)" << endl;
    cout << "Your waiter: " << waiter->getName() << endl;
    cout << "Table number: " << tableID << endl;
    cout << "Party of " << numPeople << endl;
    cout << "Items ordered:" << endl;
    order->print();
    cout << "Total: " << fixed << order->getTotal() << endl << endl;
}
