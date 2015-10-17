using StackKata;
using Xunit;

namespace UnitTest {
    public class StackTest {
        private readonly Stack _stack;

        public StackTest() {
            _stack = new Stack(10);
        }

        [Fact]
        public void IllegalCapacityThrowsIllegalCapacityException() {
            Assert.Throws<IllegalCapacityException>(() => new Stack(-1));
        }

        [Fact]
        public void CanGetSize() {
            Assert.Equal(0, _stack.Size);
            Assert.True(_stack.IsEmpty());
            _stack.Push(5);

            Assert.Equal(1, _stack.Size);
            Assert.False(_stack.IsEmpty());
        }

        [Fact]
        public void CanPopWhatIsPushed() {
            _stack.Push(10);
            _stack.Push(11);
            _stack.Push(12);
            Assert.Equal(12, _stack.Pop());
            Assert.Equal(11, _stack.Pop());
            Assert.Equal(10, _stack.Pop());
        }

        [Fact]
        public void PushOverCapacityThrowsOverflow() {
            var s = new Stack(1);
            s.Push(5);
            Assert.Throws<OverflowException>(() => s.Push(6));
        }

        [Fact]
        public void PopEmptyStackThrowsUnderFlow() {
            Assert.Throws<UnderflowException>(() => _stack.Pop());
        }

        [Fact]
        public void CanTop() {
            _stack.Push(1);
            Assert.Equal(1, _stack.Top());
            _stack.Push(2);
            Assert.Equal(2, _stack.Top());
            _stack.Push(3);
            Assert.Equal(3, _stack.Top());
        }

        [Fact]
        public void CanFindDistanceFromTop() {
            _stack.Push(1);
            _stack.Push(2);
            _stack.Push(3);
            _stack.Push(4);
            Assert.Equal(0, _stack.FindDistanceFromTop(4));
            Assert.Equal(1, _stack.FindDistanceFromTop(3));
            Assert.Equal(2, _stack.FindDistanceFromTop(2));
            Assert.Equal(3, _stack.FindDistanceFromTop(1));
            Assert.Equal(-1, _stack.FindDistanceFromTop(100));
        }

        [Fact]
        public void TopOnEmptyStackThrowsEmptyStackException() {
            Assert.Throws<EmptyException>(() => _stack.Top());
        }

        [Fact]
        public void ZeroCapacityStackBehavesAsExpected() {
            var s = new Stack(0);
            Assert.True(s.IsEmpty());
            Assert.True(s.IsFull());
            Assert.Equal(0, s.Size);
            Assert.Throws<OverflowException>(() => s.Push(1));
            Assert.Throws<UnderflowException>(() => s.Pop());
            Assert.Throws<EmptyException>(() => s.Top());
        }
    }
}