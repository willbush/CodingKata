import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class StackTest {
    private Stack stack;

    @Before
    public void setup() {
        stack = BoundedStack.make(10);
    }

    @Test(expected = Stack.IllegalCapacity.class)
    public void illegalCapacityThrowsException() {
        Stack s = BoundedStack.make(-1);
    }

    @Test
    public void canPopWhatIsPushed() {
        stack.push(5);
        stack.push(10);
        assertEquals(10, stack.pop());
        assertEquals(5, stack.pop());
    }

    @Test
    public void canGetSize() {
        assertEquals(0, stack.getSize());
        stack.push(10);
        stack.push(10);
        stack.push(10);
        assertEquals(3, stack.getSize());
    }

    @Test
    public void canGetTop() {
        stack.push(1);
        stack.push(2);
        stack.push(3);
        assertEquals(3, stack.top());
    }

    @Test
    public void canGetDistanceFromTop() {
        stack.push(1);
        stack.push(2);
        stack.push(3);
        stack.push(7);
        stack.push(8);
        assertEquals(0, stack.findDistanceFromTop(8));
        assertEquals(1, stack.findDistanceFromTop(7));
        assertEquals(2, stack.findDistanceFromTop(3));
        assertEquals(3, stack.findDistanceFromTop(2));
        assertEquals(4, stack.findDistanceFromTop(1));
    }

    @Test
    public void findDistanceFromTopWhenElementNotFoundReturnsNegativeOne() {
        assertEquals(-1, stack.findDistanceFromTop(10));
        stack.push(5);
        stack.push(15);
        assertEquals(-1, stack.findDistanceFromTop(10));
    }

    @Test(expected = BoundedStack.Overflow.class)
    public void pushOverCapacityCausesOverflow() {
        Stack s = BoundedStack.make(1);
        s.push(5);
        s.push(5);
    }

    @Test(expected = BoundedStack.Underflow.class)
    public void popOnEmptyCausesUnderflow() {
        stack.pop();
    }

    @Test(expected = BoundedStack.Underflow.class)
    public void popOnZeroCapacityStackUnderFlows() {
        Stack s = BoundedStack.make(0);
        s.pop();
    }

    @Test(expected = BoundedStack.Overflow.class)
    public void pushOnZeroCapacityStackOverFlows() {
        Stack s = BoundedStack.make(0);
        s.push(2);
    }

    @Test(expected = BoundedStack.Empty.class)
    public void topOnZeroCapacityStackCausesEmptyStackException() {
        Stack s = BoundedStack.make(0);
        s.top();
    }

    @Test
    public void zeroCapacityStackIsEmptyAndFull() {
        Stack s = BoundedStack.make(0);
        assertTrue(s.isEmpty());
        assertTrue(s.isFull());
        assertEquals(0, s.getSize());
    }

    @Test
    public void findDistanceOnZeroCapStackReturnsNegativeOne() {
        Stack s = BoundedStack.make(0);
        assertEquals(-1, s.findDistanceFromTop(1));
    }
}
