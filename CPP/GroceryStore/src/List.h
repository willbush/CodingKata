#ifndef LIST_H_
#define LIST_H_

#include <stddef.h>

template<class T>
class Node {
public:
    T data;
    Node *next;
};

template<class T>
class List {
public:
    List() {
        head = NULL;
        tail = head;
    }

    List(const List &);

    ~List();

    void add(T element);
    T removeHead(void);
    bool hasNext(void);

private:
    Node<T> *head;
    Node<T> *tail;
};

template<class T>
List<T>::List(const List &obj) {
    head = obj.head;
    tail = obj.tail;
}

template<class T>
List<T>::~List() {
    Node<T> *current = head;
    Node<T> *previous;

    while (current != NULL) {
        previous = current;
        current = current->next;
        delete previous;
    }
}

template<class T>
void List<T>::add(T element) {
    Node<T> *n = new Node<T>();
    n->data = element;

    if (head == NULL) {
        head = n;
        tail = n;
    } else {
        tail->next = n;
        tail = n;
    }
}

template<class T>
T List<T>::removeHead() {
    Node<T> *temp;

    T headData = head->data;
    temp = head->next;
    delete head;
    head = temp;
    return headData;
}

template<class T>
bool List<T>::hasNext() {
    return head != NULL;
}

#endif
