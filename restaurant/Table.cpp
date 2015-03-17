//since maxSeats is defined as const, you can set only once
//using this mechanism:
Table::Table(int tblid, int mseats) : maxSeats(mseats) {
    // maxSeats = mseats; <-- now allowed! because maxSeats is const.
}

// multiple const variables in the class:
// let us assume "const int a;" in the class too.
// Table::Table(int tblid, int mseats, int b) 
// 								: maxSeats(mseats), a(b) {

