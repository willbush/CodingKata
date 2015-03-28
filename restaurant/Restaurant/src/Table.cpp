#include "Table.h"

using namespace std;

Table::Table(int tableID, int maxSeats) :
        MAX_SEATS(maxSeats) {
    this->tableId = tableID;
    numPeople = 0;
    status = IDLE;
    waiter = NULL;
    order = NULL;
}

void Table::assignWaiter(Waiter *person) {
    waiter = person;
    status = READY;
}

void Table::seatParty(int numOfPeople) {
    if (status == READY && numOfPeople <= MAX_SEATS) {
        this->numPeople = numOfPeople;
        status = SEATED;
    }
}

void Table::partyOrdered(Order *order) {
    if (status == SEATED) {
        this->order = order;
        status = ORDERED;
    }
}

void Table::partyServed() {
    if (status == ORDERED)
        status = SERVED;
}

void Table::partyCheckout() {
    if (status == SERVED)
        status = IDLE;
}

void Table::print() const {
    cout << "table ID: " << tableId << " " << "max seats: " << MAX_SEATS << endl;
}

Table::~Table() {
    delete order;
    delete waiter;
    order = NULL;
    waiter = NULL;
}

TableStatus Table::getStatus() {
    return status;
}

int Table::getPartySize() {
    return numPeople;
}
