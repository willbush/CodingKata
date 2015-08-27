using System;
using StackKata;
using Xunit;

namespace StackTest {
    public class StackKataTest {
        private readonly IStack<int> _stack;

        public StackKataTest() {
            _stack = Stack<int>.Make(10);
        }

        [Fact]
        public void NewStack_ShouldBeEmpty() {
            Assert.True(_stack.IsEmpty);
            Assert.Equal(0, _stack.Size);
        }

        [Fact]
        public void StackPush_IncreasesSize() {
            _stack.Push(1);
            Assert.Equal(1, _stack.Size);
            _stack.Push(100);
            Assert.Equal(2, _stack.Size);
            Assert.False(_stack.IsEmpty);
        }

        [Fact]
        public void StackPop_DecreasesSize() {
            _stack.Push(1);
            _stack.Push(100);
            _stack.Pop();
            Assert.Equal(1, _stack.Size);
            _stack.Pop();
            Assert.Equal(0, _stack.Size);
            Assert.True(_stack.IsEmpty);
        }

        [Fact]
        public void WhenPushedPastCapacity_StackOverflows() {
            const int capacity = 2;
            IStack<int> stack = Stack<int>.Make(capacity);
            stack.Push(1);
            stack.Push(2);
            Exception ex = Assert.Throws<StackOverflowException>(() => stack.Push(3));
            Assert.Equal($"Stack exceeded the capacity of {capacity}", ex.Message);
        }

        [Fact]
        public void CanPopWhatIsPushed() {
            AssertCanPushThenPop(new[] { 1 });
            AssertCanPushThenPop(new[] { 1, 2 });
            AssertCanPushThenPop(new[] { 1, 2, 3, 4, 5, 6 });
        }

        private void AssertCanPushThenPop(int[] elements) {
            foreach (int x in elements)
                _stack.Push(x);

            Assert.Equal(elements.Length, _stack.Size);

            for (int i = elements.Length - 1; i >= 0; i--)
                Assert.Equal(elements[i], _stack.Pop());

            Assert.True(_stack.IsEmpty);
        }

        [Fact]
        public void NegativeCapacityCausesIllegalCapacityException() {
            const int illegalCapacity = -1;
            Exception ex = Assert.Throws<IllegalCapacityException>(() => Stack<int>.Make(illegalCapacity));
            Assert.Equal($"Cannot create stack of capacity {illegalCapacity}", ex.Message);
        }

        [Fact]
        public void CanTopWhatIsOnTop() {
            _stack.Push(1);
            Assert.Equal(1, _stack.Top());
        }

        [Fact]
        public void CanFindDistanceFromTop() {
            _stack.Push(1);
            _stack.Push(2);
            _stack.Push(8);
            _stack.Push(3);
            _stack.Push(9);
            _stack.Push(5);
            Assert.Equal(5, _stack.FindDistanceFromTop(1));
            Assert.Equal(4, _stack.FindDistanceFromTop(2));
            Assert.Equal(3, _stack.FindDistanceFromTop(8));
            Assert.Equal(2, _stack.FindDistanceFromTop(3));
            Assert.Equal(1, _stack.FindDistanceFromTop(9));
            Assert.Equal(0, _stack.FindDistanceFromTop(5));
            Assert.Null(_stack.FindDistanceFromTop(100));
        }

        [Fact]
        public void CanCheckIfStackContainsElements() {
            _stack.Push(1);
            _stack.Push(2);
            _stack.Push(8);
            _stack.Push(3);
            _stack.Push(9);
            _stack.Push(5);
            Assert.True(_stack.Contains(1));
            Assert.True(_stack.Contains(2));
            Assert.True(_stack.Contains(8));
            Assert.True(_stack.Contains(3));
            Assert.True(_stack.Contains(9));
            Assert.True(_stack.Contains(5));
            Assert.False(_stack.Contains(100));
        }
    }
}