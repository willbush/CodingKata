#ifndef LOOKUPTABLE_H
#define LOOKUPTABLE_H

#include <iostream>

using namespace std;

#define MAX_RANGE 10

template<class T>
class LookupTable {
public:
    LookupTable();
    ~LookupTable();

    void addRange(int start, int end);
    T &operator[](int value);
private:
    T *aptr[MAX_RANGE];
    int rangeStart[MAX_RANGE];
    int rangeEnd[MAX_RANGE];
    int numOfRanges;
    T defaultValue; //need to return when [] operator does not find a valid product
};

template<class T>
LookupTable<T>::LookupTable() {
    numOfRanges = 0;
}

template<class T>
void LookupTable<T>::addRange(int start, int end) {
    rangeStart[numOfRanges] = start;
    rangeEnd[numOfRanges] = end;
    aptr[numOfRanges] = new T[end - start + 1];
    numOfRanges++;
}

template<class T>
T &LookupTable<T>::operator[](int value) {
    // find the range
    // if the valid range is found
    //   return aptr[i][value - rangeStart[i]];
    // else
    //  return defaultValue;
}

template<class T>
LookupTable<T>::~LookupTable() {
    // use a loop for in numOfRanges
    // delete [] aptr[i];
}

#endif
