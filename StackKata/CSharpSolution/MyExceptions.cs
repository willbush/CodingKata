using System;

namespace StackKata {
	public abstract class MyExceptions {

		[Serializable]
		public class OverflowException : Exception {
			public OverflowException() {
			}
		}

		[Serializable]
		public class UnderflowException : Exception {
			public UnderflowException() {
			}
		}

		[Serializable]
		public class IllegalCapacityException : Exception {
			public IllegalCapacityException() {
			}
		}

		[Serializable]
		public class EmptyException : Exception {
			public EmptyException() {
			}
		}
	}
}
