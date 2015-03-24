#include <gtest/gtest.h>
#include "../Waiter.cpp"
#include "../Order.cpp"
#include "../Table.cpp"

TEST(emptyTable, shouldBeIDLE) {
    int maxSeats = 5;
    Table *t = new Table(1, 5);

    ASSERT_EQ(IDLE, t->getStatus());
}

TEST(tableWithWaiter, shouldBeREADY) {
    int maxSeats = 5;
    Table *t = new Table(1, 5);
    Waiter *w = new Waiter("john", "", NULL);
    t->assignWaiter(w);

    ASSERT_EQ(READY, t->getStatus());
}

TEST(trySeatTable, withNoWaiterFails) {
    int maxSeats = 5;
    Table *t = new Table(1, maxSeats);
    t->seatParty(2);

    ASSERT_EQ(IDLE, t->getStatus());
    ASSERT_EQ(0, t->getPartySize());
}

TEST(canSeat, table) {
    int maxSeats = 5;
    Table *t = new Table(1, maxSeats);
    Waiter *w = new Waiter("john", "", NULL);
    t->assignWaiter(w);

    // party cannot exceed max seats
    t->seatParty(10);
    ASSERT_EQ(READY, t->getStatus());
    ASSERT_EQ(0, t->getPartySize());

    t->seatParty(2);
    ASSERT_EQ(SEATED, t->getStatus());
    ASSERT_EQ(2, t->getPartySize());
    // cannot seat two parties
    t->seatParty(3);
    ASSERT_EQ(2, t->getPartySize());
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

