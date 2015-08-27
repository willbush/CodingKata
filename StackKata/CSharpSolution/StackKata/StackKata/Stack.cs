using System;
using System.Linq;

namespace StackKata {
    public interface IStack<T> {
        bool IsEmpty { get; }
        int Size { get; }
        void Push(T element);
        T Pop();
        T Top();
        int? FindDistanceFromTop(T element);
        bool Contains(T element);
    }

    public class Stack<T> : IStack<T> {
        private readonly int _capacity;
        private readonly T[] _elements;

        public static IStack<T> Make(int capacity) {
            if (capacity < 0)
                throw new IllegalCapacityException($"Cannot create stack of capacity {capacity}");
            if (capacity == 0)
                return new ZeroCapacityStack<T>();

            return new Stack<T>(capacity);
        }

        private Stack(int capacity) {
            _capacity = capacity;
            _elements = new T[capacity];
        }

        public bool IsEmpty => Size == 0;

        public int Size { get; private set; }

        public void Push(T element) {
            if (Size == _capacity)
                throw new StackOverflowException($"Stack exceeded the capacity of {_capacity}");

            _elements[Size++] = element;
        }

        public T Pop() {
            if (IsEmpty)
                throw new StackUnderflowException("Cannot pop the stack because it is empty");

            return _elements[--Size];
        }

        public T Top() {
            return _elements[Size - 1];
        }

        public int? FindDistanceFromTop(T element) {
            if (element == null) return null;

            for (int i = 0; i < _elements.Length; i++)
                if (_elements[i].Equals(element))
                    return (Size - 1) - i;

            return null;
        }

        public bool Contains(T element) {
            return _elements.Contains(element);
        }
    }

    public class ZeroCapacityStack<T> : IStack<T> {
        public bool IsEmpty { get; } = true;
        public int Size { get; } = 0;

        public void Push(T element) {
            throw new StackOverflowException("Stack exceeded the capacity of 0");
        }

        public T Pop() {
            throw new StackUnderflowException("Cannot pop empty stack.");
        }

        public T Top() {
            throw new StackEmptyException("Cannot top empty stack.");
        }

        public int? FindDistanceFromTop(T element) {
            return null;
        }

        public bool Contains(T element) {
            return false;
        }
    }

    public class StackUnderflowException : Exception {
        public StackUnderflowException() {}
        public StackUnderflowException(string message) : base(message) {}
    }

    public class IllegalCapacityException : Exception {
        public IllegalCapacityException() {}
        public IllegalCapacityException(string message) : base(message) {}
    }

    public class StackEmptyException : Exception {
        public StackEmptyException() {}
        public StackEmptyException(string message) : base(message) {}
    }
}