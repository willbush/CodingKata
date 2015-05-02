#ifndef LIST_H_
#define LIST_H_

template<class T>
struct Node {
    T data;
    T *next;
};

template<class T>
class List {
public:
    void add(const T &element);
    T removeHead(void);
    bool hasNext(void);

private:
    Node<T> *head;
};

#endif
