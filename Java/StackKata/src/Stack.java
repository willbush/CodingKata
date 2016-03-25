public interface Stack {
    void push(int element);

    int pop();

    int top();

    int findDistanceFromTop(int element);

    int getSize();

    boolean isEmpty();

    boolean isFull();


    class Overflow extends RuntimeException {
    }

    class Underflow extends RuntimeException {
    }

    class Empty extends RuntimeException {
    }

    class IllegalCapacity extends RuntimeException {
    }
}
