using System;

namespace StackKata {
	public class Stack : IStack {
		private int size;
		private int capacity;
		private int[] elements;

		public static IStack Make(int capacity) {
			if (capacity < 0) {
				throw new MyExceptions.IllegalCapacityException();
			} else if (capacity == 0) {
				return new ZeroCapacityStack();
			} else {
				return new Stack(capacity);
			}
		}

		Stack(int capacity) {
			this.capacity = capacity;
			elements = new int[capacity];
		}

		public void push(int element) {
			if (capacity == size) {
				throw new MyExceptions.OverflowException();
			}
			elements[size++] = element;
		}

		public int pop() {
			if (isEmpty()) {
				throw new MyExceptions.UnderflowException();
			}
			return elements[--size];
		}

		public int peek() {
			if (isEmpty()) {
				throw new MyExceptions.EmptyException();
			}
			return elements[size - 1];
		}

		public int? find(int element) {
			for (int i = size - 1; i >= 0; i--) {
				if (elements[i] == element) {
					return (size - 1) - i;
				}
			}
			return null;
		}

		public Boolean isEmpty() {
			return size == 0;
		}

		public int getSize() {
			return size;
		}
	}
}
