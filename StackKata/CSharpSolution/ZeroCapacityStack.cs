using System;

namespace StackKata {
	public class ZeroCapacityStack : IStack {

		public void push(int element) {
			throw new MyExceptions.OverflowException();
		}

		public int pop() {
			throw new MyExceptions.UnderflowException();
		}

		public int peek() {
			throw new MyExceptions.EmptyException();
		}

		public int? find(int element) {
			return null;
		}

		public Boolean isEmpty() {
			return true;
		}

		public int getSize() {
			return 0;
		}
	}
}
