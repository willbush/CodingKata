using Microsoft.VisualStudio.TestTools.UnitTesting;
using StackKata;

namespace StackKataTest {
	[TestClass]
	public class Test {
		IStack stack = Stack.Make(2);

		[TestMethod]
		public void testIfNewStackIsEmpty() {
			Assert.IsTrue(stack.isEmpty());
		}

		[TestMethod]
		public void testForSizeOne_afterOnePush() {
			stack.push(1);
			Assert.AreEqual(1, stack.getSize());
		}

		[TestMethod]
		public void testForEmptyStack_afterPushAndPop() {
			stack.push(1);
			stack.pop();
			Assert.IsTrue(stack.isEmpty());
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.OverflowException))]
		public void pushPastTheLimit() {
			stack.push(1);
			stack.push(1);
			stack.push(1);
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.UnderflowException))]
		public void popAnEmptyStack() {
			stack.pop();
		}

		[TestMethod]
		public void whenOneIsPushed_popReturnsOne() {
			stack.push(1);
			Assert.AreEqual(1, stack.pop());
		}

		[TestMethod]
		public void whenOneAndTwoIsPushed_popingReturnsOneAndTwo() {
			stack.push(1);
			stack.push(2);
			Assert.AreEqual(2, stack.pop());
			Assert.AreEqual(1, stack.pop());
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.IllegalCapacityException))]
		public void testNegativeStackSize() {
			stack = Stack.Make(-1);
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.OverflowException))]
		public void pushZeroCapacityStackPastLimit() {
			stack = Stack.Make(0);
			stack.push(1);
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.UnderflowException))]
		public void popZeroCapacityStack() {
			stack = Stack.Make(0);
			stack.pop();
		}

		[TestMethod]
		public void checkZeroCapacityStackSize() {
			stack = Stack.Make(0);
			Assert.IsTrue(stack.isEmpty());
			Assert.AreEqual(0, stack.getSize());
		}

		[TestMethod]
		public void pushOneAndPeek() {
			stack.push(1);
			Assert.AreEqual(1, stack.peek());
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.EmptyException))]
		public void peekAnEmptyStack() {
			stack.peek();
		}

		[TestMethod]
		[ExpectedException(typeof(MyExceptions.EmptyException))]
		public void peekZeroCapacityStack() {
			stack = Stack.Make(0);
			stack.peek();
		}


		[TestMethod]
		public void canFindElement() {
			stack.push(1);
			stack.push(2);
			int? indexOfOne = stack.find(1);
			int? indexOfTwo = stack.find(2);
			Assert.AreEqual(1, indexOfOne);
			Assert.AreEqual(0, indexOfTwo);
		}

		[TestMethod]
		public void findOnEmptyStackReturnsNull() {
			stack.find(1);
		}

		[TestMethod]
		public void findOnZeroCapacityStackReturnsNull() {
			stack = Stack.Make(0);
			stack.find(1);
		}
	}
}
