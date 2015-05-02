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
    T *tablePtr[MAX_RANGE];
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
    tablePtr[numOfRanges] = new T[end - start + 1];
    numOfRanges++;
}

template<class T>
T &LookupTable<T>::operator[](int value) {
    int foundStartRange = -1;
    int rangeIndex = -1;

    for (int i = 0; i < numOfRanges; i++) {
        if (value >= rangeStart[i] && value <= rangeEnd[i]) {
            foundStartRange = rangeStart[i];
            rangeIndex = i;
            break;
        }
    }

    if (foundStartRange > -1)
        return tablePtr[rangeIndex][value - foundStartRange];
    else
        return defaultValue;
}

template<class T>
LookupTable<T>::~LookupTable() {
    for (int i = 0; i < numOfRanges; i++)
        delete[] tablePtr[i];
}

#endif
