package FizzBuzzKata;

public class FizzBuzz {
	private int boundingValue;

	public static void main(String[] args) {
		FizzBuzz fb = new FizzBuzz(50);
		FizzBuzz fb2 = new FizzBuzz(-50);
		fb.printResults();
		System.out.println("negative case:");
		fb2.printResults();
	}

	public FizzBuzz() {
	}

	public FizzBuzz(int boundingValue) {
		this.boundingValue = boundingValue;
	}

	public void printResults() {
		if (boundingValue > 0) {
			for (int i = 1; i <= boundingValue; i++) {
				System.out.println(evaluate(i));
			}
		} else {
			for (int i = -1; i >= boundingValue; i--) {
				System.out.println(evaluate(i));
			}
		}
	}

	public String evaluate(int n) {
		if (isMultipleOf3(n) && isMultipleOf5(n)) {
			return "FizzBuzz";
		} else if (isMultipleOf3(n)) {
			return "Fizz";
		} else if (isMultipleOf5(n)) {
			return "Buzz";
		} else {
			return Integer.toString(n);
		}
	}

	private boolean isMultipleOf5(int n) {
		return n % 5 == 0;
	}

	private boolean isMultipleOf3(int n) {
		return n % 3 == 0;
	}
}
