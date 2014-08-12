using System;

namespace StackKata {
	public interface IStack {

		void push(int element);

		int pop();

		int peek();

		int? find(int element);

		Boolean isEmpty();

		int getSize();
	}
}
