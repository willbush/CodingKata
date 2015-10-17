using System;

namespace StackKata {
    public class Stack {
        private readonly int _capacity;
        private readonly int[] _elements;
        public int Size { get; private set; }

        public Stack(int capacity) {
            if (capacity < 0)
                throw new IllegalCapacityException();
            _capacity = capacity;
            _elements = new int[capacity];
        }

        public void Push(int element) {
            if (IsFull())
                throw new OverflowException();
            _elements[Size++] = element;
        }

        public bool IsFull() {
            return Size == _capacity;
        }

        public int Pop() {
            if (IsEmpty())
                throw new UnderflowException();
            return _elements[--Size];
        }

        public int Top() {
            if (IsEmpty())
                throw new EmptyException();
            return _elements[Size - 1];
        }

        public bool IsEmpty() {
            return Size == 0;
        }

        public int FindDistanceFromTop(int element) {
            for (int i = Size - 1; i >= 0; i--)
                if (_elements[i] == element)
                    return (Size - 1) - i;

            return -1;
        }
    }

    public class OverflowException : Exception {}

    public class UnderflowException : Exception {}

    public class EmptyException : Exception {}

    public class IllegalCapacityException : Exception {}
}