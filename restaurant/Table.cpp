#include "Table.h"

Table::Table(int tableID, int maxSeats) : MAX_SEATS(maxSeats) {
    this->tableId = tableID;
}

void Table::assignWaiter(Waiter *person) {
    waiter = person;
}

void Table::partySeated(int numOfPeople) {

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
    delete [] order;
    delete [] waiter;
}
