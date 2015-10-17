public class ZeroCapacityStack implements Stack {
    @Override
    public void push(int element) {
        throw new Overflow();
    }

    @Override
    public int pop() {
        throw new Underflow();
    }

    @Override
    public int top() {
        throw new Empty();
    }

    @Override
    public int findDistanceFromTop(int element) {
        return -1;
    }

    @Override
    public int getSize() {
        return 0;
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public boolean isFull() {
        return true;
    }
}
