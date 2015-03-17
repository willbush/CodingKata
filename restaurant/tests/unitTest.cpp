#include "../SeatSelection.cpp"
#include <gtest/gtest.h>

void fillAllSectionsExceptOne(SeatSelection & p, SeatSelection & r);


TEST(PremiumTest, 8leftThen2Right) {
    SeatSelection premium(1, 5);

    ASSERT_EQ("1A 1B 1C 1D 1E 1F 1G 1H \n\n",
            premium.findSeats(8));
    ASSERT_EQ("2A 2B 2C 2D 2E 2F 2G 2H \n\n",
            premium.findSeats(8));
    ASSERT_EQ("3A 3B 3C 3D 3E 3F 3G 3H \n\n",
            premium.findSeats(8));
    ASSERT_EQ("4A 4B 4C 4D 4E 4F 4G 4H \n\n",
            premium.findSeats(8));
    ASSERT_EQ("5A 5B 5C 5D 5E 5F 5G 5H \n\n",
            premium.findSeats(8));
    ASSERT_EQ("1I 1J 2I 2J 3I 3J 4I 4J \n\n",
            premium.findSeats(8));
    ASSERT_EQ("5I 5J \n\n", // last remaining seats
            premium.findSeats(2));
    ASSERT_EQ("", premium.findSeats(1)); // no one seated
}

TEST(RegularTest, 8leftThen2Right) {
    SeatSelection regular(6, 15);

    ASSERT_EQ("6A 6B 6C 6D 6E 6F 6G 6H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("7A 7B 7C 7D 7E 7F 7G 7H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("8A 8B 8C 8D 8E 8F 8G 8H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("9A 9B 9C 9D 9E 9F 9G 9H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("10A 10B 10C 10D 10E 10F 10G 10H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("11A 11B 11C 11D 11E 11F 11G 11H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("12A 12B 12C 12D 12E 12F 12G 12H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("13A 13B 13C 13D 13E 13F 13G 13H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("14A 14B 14C 14D 14E 14F 14G 14H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("15A 15B 15C 15D 15E 15F 15G 15H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("16A 16B 16C 16D 16E 16F 16G 16H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("17A 17B 17C 17D 17E 17F 17G 17H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("18A 18B 18C 18D 18E 18F 18G 18H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("19A 19B 19C 19D 19E 19F 19G 19H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("20A 20B 20C 20D 20E 20F 20G 20H \n\n",
            regular.findSeats(8));
    ASSERT_EQ("6I 6J 7I 7J 8I 8J 9I 9J \n\n",
            regular.findSeats(8));
    ASSERT_EQ("10I 10J 11I 11J 12I 12J 13I 13J \n\n",
            regular.findSeats(8));
    ASSERT_EQ("14I 14J 15I 15J 16I 16J 17I 17J \n\n",
            regular.findSeats(8));
    ASSERT_EQ("18I 18J 19I 19J 20I 20J \n\n", // last remaining seats
            regular.findSeats(6));
    ASSERT_EQ("", regular.findSeats(1)); // no one seated
}

TEST(FillTest, fillThenTryToAddOne) {
    SeatSelection premium(1, 5);
    SeatSelection regular(6, 15);
    fillAllSectionsExceptOne(premium, regular);

    ASSERT_EQ("", premium.findSeats(2)); // only one seat left
    ASSERT_EQ("", regular.findSeats(2));
    ASSERT_EQ("5J \n\n", premium.findSeats(1)); // seat last person
    ASSERT_EQ("20J \n\n", regular.findSeats(1));
    ASSERT_EQ("", premium.findSeats(1)); // should be full
    ASSERT_EQ("", regular.findSeats(1));
}

void fillAllSectionsExceptOne(SeatSelection & p, SeatSelection & r) {
    // fills all seats up to the last row
    for (int i = 0; i < 4; i++)
        p.findSeats(10);
    p.findSeats(9); // fills all seats in last row except one.

    for (int i = 0; i < 14; i++)
        r.findSeats(10);
    r.findSeats(9);
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

