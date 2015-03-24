#include "Table.h"

Table::Table(int tableID, int maxSeats) : MAX_SEATS(maxSeats) {
    this->tableId = tableID;
    numPeople = 0;
    status = IDLE;
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
    this->order = order;
}

void Table::partyServed() {

}

void Table::partyCheckout() {

}

void Table::print() const {
    cout << "table ID: " << tableId << " "
            << "max seats: " << MAX_SEATS << endl;
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
