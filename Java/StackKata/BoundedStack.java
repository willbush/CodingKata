public class BoundedStack implements Stack {
    private int capacity;
    private int size = 0;
    private int[] elements;

    public static Stack make(int capacity) {
        if (capacity < 0)
            throw new IllegalCapacity();
        else if (capacity == 0)
            return new ZeroCapacityStack();
        else
            return new BoundedStack(capacity);
    }

    private BoundedStack(int capacity) {
        this.capacity = capacity;
        elements = new int[capacity];
    }

    @Override
    public void push(int element) {
        if (isFull())
            throw new Overflow();
        elements[size++] = element;
    }

    @Override
    public int pop() {
        if (isEmpty())
            throw new Underflow();
        return elements[--size];
    }

    @Override
    public int getSize() {
        return size;
    }

    @Override
    public int top() {
        if (isEmpty())
            throw new Empty();
        return elements[size - 1];
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public boolean isFull() {
        return size == capacity;
    }

    @Override
    public int findDistanceFromTop(int element) {
        for (int i = size - 1; i >= 0; i--)
            if (elements[i] == element)
                return (size - 1) - i;

        return -1;
    }
}
