using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
                    Console.WriteLine(evaluate(i));
                }
            } else {
                for (int i = -1; i >= boundingValue; i--) {
                    Console.WriteLine(evaluate(i));
                }
            }
        }

        public string evaluate(int n) {
            if (isMultipleOf3(n) && isMultipleOf5(n)) {
                return "FizzBuzz";
            } else if (isMultipleOf3(n)) {
                return "Fizz";
            } else if (isMultipleOf5(n)) {
                return "Buzz";
            } else {
                return n.ToString();
            }
        }

        private static bool isMultipleOf3(int n) {
            return n % 3 == 0;
        }

        private static bool isMultipleOf5(int n) {
            return n % 5 == 0;
        }
    }
}
