#ifndef SIMPLEVECTOR_H
#define SIMPLEVECTOR_H

#include <iostream>
#include <cstdlib>
using namespace std;

template<class T>
class SimpleVector {

public:
    SimpleVector(int);
    SimpleVector(const SimpleVector &);
    ~SimpleVector();

    int size() {
        return arraySize;
    }

    T &operator[](int);
    void printAllElements();

private:
    T *aptr;
    int arraySize;
    void displaySubscriptError();
};

template<class T>
SimpleVector<T>::SimpleVector(int size) {
    arraySize = size;
    aptr = new T[size];

    for (int count = 0; count < arraySize; count++)
        aptr[count] = T();
}

// Copy Constructor for SimpleVector class
template<class T>
SimpleVector<T>::SimpleVector(const SimpleVector &obj) {
    arraySize = obj.arraySize;
    aptr = new T[arraySize];

    for (int count = 0; count < arraySize; count++)
        aptr[count] = obj[count];
}

template<class T>
SimpleVector<T>::~SimpleVector() {
    if (arraySize > 0)
        delete[] aptr;
}

template<class T>
T &SimpleVector<T>::operator[](int subcript) {
    if (subcript < 0 || subcript >= arraySize)
        displaySubscriptError();

    return aptr[subcript];
}

template<class T>
void SimpleVector<T>::displaySubscriptError() {
    cout << "ERROR: Subscript out of range.\n";
    exit(0);
}

template<class T>
void SimpleVector<T>::printAllElements() {
    for (int i = 0; i < arraySize; i++)
        cout << aptr[i] << "  ";

    cout << endl;
}

#endif
