package Stack;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class StackTest {

	private Stack stack;

	@Before
	public void setUp() throws Exception {
		stack = BoundedStack.Make(2);
	}

	@Test
	public void testIfNewStackIsEmpty() throws Exception {
		assertTrue(stack.isEmpty());
		assertEquals(0, stack.getSize());
	}

	@Test
	public void testForSizeOne_afterOnePush() throws Exception {
		stack.push(1);
		assertEquals(1, stack.getSize());
		assertFalse(stack.isEmpty());
	}

	@Test
	public void testForEmptyStack_afterOnePushAndPop() throws Exception {
		stack.push(1);
		stack.pop();
		assertTrue(stack.isEmpty());
	}

	@Test(expected = BoundedStack.Overflow.class)
	public void pushPastTheLimit() throws Exception {
		stack.push(1);
		stack.push(1);
		stack.push(1);
	}

	@Test(expected = BoundedStack.Underflow.class)
	public void popAnEmptyStack() throws Exception {
		stack.pop();
	}

	@Test
	public void whenOneIsPushed_oneIsPopped() throws Exception {
		stack.push(1);
		assertEquals(1, stack.pop());
	}

	@Test
	public void whenOneAndTwoArePushed_twoAndOneArePopped() throws Exception {
		stack.push(1);
		stack.push(2);
		assertEquals(2, stack.pop());
		assertEquals(1, stack.pop());
	}

	@Test(expected = BoundedStack.IllegalCapacity.class)
	public void testNegativeSizeStack() throws Exception {
		BoundedStack.Make(-1);
	}

	@Test(expected = BoundedStack.Overflow.class)
	public void pushZeroCapacityStackPastLimit() throws Exception {
		stack = BoundedStack.Make(0);
		stack.push(1);
	}

	@Test
	public void checkZeroCapacitySize() {
		stack = BoundedStack.Make(0);
		assertTrue(stack.isEmpty());
		assertEquals(0, stack.getSize());
	}

	@Test(expected = BoundedStack.Underflow.class)
	public void popZeroCapacityStack() throws Exception {
		stack = BoundedStack.Make(0);
		stack.pop();
	}

	@Test
	public void pushOneAndCheckTop() throws Exception {
		stack.push(1);
		assertEquals(1, stack.top());
	}

	@Test(expected = BoundedStack.Empty.class)
	public void testTopOnEmptyStack() throws Exception {
		stack.top();
	}

	@Test(expected = BoundedStack.Empty.class)
	public void testTopOnZeroCapacityStack() throws Exception {
		stack = BoundedStack.Make(0);
		stack.top();
	}

	@Test
	public void canFindElement() throws Exception {
		stack.push(1);
		stack.push(2);
		int indexOne = stack.find(1);
		int indexTwo = stack.find(2);
		assertEquals(1, indexOne);
		assertEquals(0, indexTwo);
	}

	@Test
	public void findOnEmptyStackReturnsNull() throws Exception {
		assertNull(stack.find(2));
	}

	@Test
	public void findOnZeroStackReturnsNull() throws Exception {
		stack = BoundedStack.Make(0);
		assertNull(stack.find(2));
	}
}
