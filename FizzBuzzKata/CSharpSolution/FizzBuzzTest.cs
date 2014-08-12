using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FizzBuzzKata;

namespace FizzBuzzTest {

	[TestClass]
	public class FizzBuzzTest {
		FizzBuzz fb = new FizzBuzz();

		[TestMethod]
		public void zeroReturnsFizzBuzz() {
			Assert.AreEqual("FizzBuzz", fb.evaluate(0));
		}

		[TestMethod]
		public void oneReturnsOne() {
			Assert.AreEqual("1", fb.evaluate(1));
		}

		[TestMethod]
		public void threeReturnsFizz() {
			Assert.AreEqual("Fizz", fb.evaluate(3));
		}

		[TestMethod]
		public void fiveReturnsBuzz() {
			Assert.AreEqual("Buzz", fb.evaluate(5));
		}

		[TestMethod]
		public void sevenReturns7() {
			Assert.AreEqual("7", fb.evaluate(7));
		}

		[TestMethod]
		public void tenReturnsBuzz() {
			Assert.AreEqual("Buzz", fb.evaluate(10));
		}

		[TestMethod]
		public void fifteenReturnsFizzBuzz() {
			Assert.AreEqual("FizzBuzz", fb.evaluate(15));
		}

		[TestMethod]
		public void fourtyFiveReturnsFizzBuzz() {
			Assert.AreEqual("FizzBuzz", fb.evaluate(45));
		}
	}
}
