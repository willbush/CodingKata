using System;
using StackKata;
using Xunit;

namespace StackTest {
    public class ZeroCapacityStackTest {
        private readonly IStack<int> _stack;

        public ZeroCapacityStackTest() {
            _stack = Stack<int>.Make(0);
        }

        [Fact]
        public void ZeroCapacityStackIsEmpty() {
            Assert.True(_stack.IsEmpty);
            Assert.Equal(0, _stack.Size);
        }

        [Fact]
        public void WhenZeroCapacityStackIsPushed_StackOverFlows() {
            Exception ex = Assert.Throws<StackOverflowException>(() => _stack.Push(1));
            Assert.Equal("Stack exceeded the capacity of 0", ex.Message);
        }

        [Fact]
        public void WhenZeroCapacityStackIsPopped_StackUnderFlows() {
            Exception ex = Assert.Throws<StackUnderflowException>(() => _stack.Pop());
            Assert.Equal("Cannot pop empty stack.", ex.Message);
        }

        [Fact]
        public void WhenZeroCapacityStackIsTopped_ThrowsStackEmptyException() {
            Exception ex = Assert.Throws<StackEmptyException>(() => _stack.Top());
            Assert.Equal("Cannot top empty stack.", ex.Message);
        }

        [Fact]
        public void FindOnZeroCapacityStack_FindReturnsNull() {
            Assert.Null(_stack.FindDistanceFromTop(5));
        }

        [Fact]
        public void FindOnZeroCapacityStack_ContainsReturnsFalse() {
            Assert.False(_stack.Contains(1));
        }
    }
}