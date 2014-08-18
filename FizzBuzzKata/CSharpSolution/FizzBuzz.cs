using System;

namespace FizzBuzzKata {
	public class FizzBuzz {
		private int boundingValue;

		static void Main() {
			FizzBuzz fb = new FizzBuzz(100);
			FizzBuzz fb2 = new FizzBuzz(-100);
			fb.printResults();
			Console.WriteLine("Negative case:");
			fb2.printResults();
		}

		public FizzBuzz() { }

		public FizzBuzz(int value) {
			this.boundingValue = value;
		}

		public void printResults() {
			if (boundingValue > 0) {
				for (int i = 1; i <= boundingValue; i++) {
					Console.WriteLine(eval(i));
				}
			} else {
				for (int i = -1; i >= boundingValue; i--) {
					Console.WriteLine(eval(i));
				}
			}
		}

		public string eval(int n) {
			string result = "";

			if (isMultipleOf3(n)) {
				result += "Fizz";
			}
			if (isMultipleOf5(n)) {
				result += "Buzz";
			}
			if (result == "") {
				result += n;
			}
			return result;
		}

		private static bool isMultipleOf3(int n) {
			return n % 3 == 0;
		}

		private static bool isMultipleOf5(int n) {
			return n % 5 == 0;
		}
	}
}
