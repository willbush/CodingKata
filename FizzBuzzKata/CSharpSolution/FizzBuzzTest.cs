using Microsoft.VisualStudio.TestTools.UnitTesting;
using FizzBuzzKata;

namespace FizzBuzzTest {

	[TestClass]
	public class FizzBuzzTest {
		FizzBuzz fb = new FizzBuzz();

		[TestMethod]
		public void eval_zero_returnsFizzBuzz() {
			Assert.AreEqual("FizzBuzz", fb.eval(0));
		}

		[TestMethod]
		public void eval_one_returnsOne() {
			Assert.AreEqual("1", fb.eval(1));
		}

		[TestMethod]
		public void eval_three_returnsFizz() {
			Assert.AreEqual("Fizz", fb.eval(3));
		}

		[TestMethod]
		public void eval_five_returnsBuzz() {
			Assert.AreEqual("Buzz", fb.eval(5));
		}

		[TestMethod]
		public void eval_fifteen_returnsFizzBuzz() {
			Assert.AreEqual("FizzBuzz", fb.eval(15));
		}
	}
}
