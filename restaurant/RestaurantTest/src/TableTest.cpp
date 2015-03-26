#include "TableTest.h"

#include "cute.h"
#include "Table.cpp"
#include "Order.cpp"
#include "Waiter.cpp"

const int maxSeats { 5 };
Table table(1, maxSeats);

void emptyTableIsIdle() {
    ASSERT_EQUAL(IDLE, table.getStatus());
}

void seatingAnIdleTableFails() {
    table.seatParty(2);
    ASSERT_EQUAL(0, table.getPartySize());
    ASSERT_EQUAL(IDLE, table.getStatus());
}

void tableWithWaiterIsRead() {
    Waiter *waiter = new Waiter { "John", "", 0 };
    Table t(1, maxSeats);
    t.assignWaiter(waiter);
    ASSERT_EQUAL(READY, t.getStatus());
}

void canSeatTable() {
    Table t(1, maxSeats);
    Waiter *waiter = new Waiter { "John", "", 0 };
    t.assignWaiter(waiter);

    t.seatParty(10);
    ASSERT_EQUAL(0, t.getPartySize());

    t.seatParty(2);
    ASSERT_EQUAL(2, t.getPartySize());
    ASSERT_EQUAL(SEATED, t.getStatus());
    // cannot seat another party once table is seated.
    t.seatParty(4);
    ASSERT_EQUAL(2, t.getPartySize());
}

void canOrder() {
    Table t(1, maxSeats);
    Waiter *waiter = new Waiter { "John", "", 0 };
    t.assignWaiter(waiter);
    t.seatParty(2);
    ASSERT_EQUAL(2, t.getPartySize());
    Order *order = new Order { "", 0 };

    ASSERT_EQUAL(SEATED, t.getStatus());
    t.partyOrdered(order);
    ASSERT_EQUAL(ORDERED, t.getStatus());
}

void canServe() {
    Table t(1, maxSeats);
    Waiter *waiter = new Waiter { "John", "", 0 };
    t.assignWaiter(waiter);
    t.seatParty(2);
    Order *order = new Order { "", 0 };

    ASSERT_EQUAL(SEATED, t.getStatus());
    t.partyOrdered(order);
    ASSERT_EQUAL(ORDERED, t.getStatus());
    t.partyServed();
    ASSERT_EQUAL(SERVED, t.getStatus());
}

void serveFailsIfNotOrdered() {
    Table t(1, maxSeats);
    Waiter *waiter = new Waiter { "John", "", 0 };
    t.assignWaiter(waiter);
    t.seatParty(2);

    t.partyServed();
    ASSERT_EQUAL(SEATED, t.getStatus());
}

cute::suite makeSuiteTableTest() {
    cute::suite s;
    s.push_back(CUTE(emptyTableIsIdle));
    s.push_back(CUTE(seatingAnIdleTableFails));
    s.push_back(CUTE(tableWithWaiterIsRead));
    s.push_back(CUTE(canSeatTable));
    s.push_back(CUTE(canOrder));
    s.push_back(CUTE(canServe));
	s.push_back(CUTE(serveFailsIfNotOrdered));
    return s;
}
