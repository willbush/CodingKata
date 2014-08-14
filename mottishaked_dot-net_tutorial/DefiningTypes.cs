using System;

class Program {

	static void Main() {
		Banana[] bananas = new Banana[3];
		bananas[0] = new Banana(112);
		bananas[1] = new Banana(140);
		bananas[2] = new Banana(FruitAttribs.Sweet | FruitAttribs.Soft, 168);

		printWeights(bananas);
		countSpoiledBananas(bananas);
	}

	private static void printWeights(Banana[] bananas) {
		FruitUtility.printMetricWeight(bananas);
		FruitUtility.printImperialWeight(bananas);
	}

	private static void countSpoiledBananas(Banana[] bananas) {
		int spoiledCount = 0;
		foreach (Banana banana in bananas) {
			if (banana.isSpoiled()) {
				spoiledCount++;
			}
		}
		Console.WriteLine("there are {0} spoiled bananas.", spoiledCount);
	}
}

[Flags]
enum FruitAttribs {
	None = 0x00,
	Sweet = 0x01,
	Sour = 0x02,
	Firm = 0x03,
	Soft = 0x04
}

interface IMetricWeight {
	double getWeight();
}

interface IImperialWeight {
	double getWeight();
}

class Fruit : IImperialWeight, IMetricWeight {
	string name;
	FruitAttribs fruitAttribs;
	double weightGrams;

	public Fruit(string name, FruitAttribs fruitAttribs, double weightGrams) {
		this.name = name;
		this.fruitAttribs = fruitAttribs;
		this.weightGrams = weightGrams;
	}

	double IMetricWeight.getWeight() {
		return this.weightGrams;
	}

	double IImperialWeight.getWeight() {
		return this.weightGrams / 28;
	}
}

class Banana : Fruit {
	FruitAttribs fruitAttribs;

	public Banana(FruitAttribs fruitAttribs, double weightGrams)
		: base("Banana", fruitAttribs, weightGrams) {
		this.fruitAttribs = fruitAttribs;
	}

	public Banana(double weightGrams)
		: this(FruitAttribs.Sweet | FruitAttribs.Firm, weightGrams) {
	}

	public bool isSpoiled() {
		bool isSoft = (this.fruitAttribs & FruitAttribs.Soft) == FruitAttribs.Soft;
		bool isSour = (this.fruitAttribs & FruitAttribs.Sour) == FruitAttribs.Sour;
		return isSoft || isSour;
	}
}


static class FruitUtility {

	public static void printMetricWeight(Fruit[] fruits) {
		Console.WriteLine("Fruit weight in grams:");

		foreach (Fruit fruit in fruits) {
			IMetricWeight mw = fruit as IMetricWeight;
			if (mw != null) {
				Console.WriteLine(mw.getWeight());
			}
		}
	}

	public static void printImperialWeight(Fruit[] fruits) {
		Console.WriteLine("Fruit weight in OZ:");

		foreach (Fruit fruit in fruits) {
			IImperialWeight iw = fruit as IImperialWeight;
			if (iw != null) {
				Console.WriteLine(iw.getWeight());
			}
		}
	}
}
