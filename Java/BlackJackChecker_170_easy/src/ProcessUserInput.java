import java.io.InputStream;
import java.util.Scanner;

public final class ProcessUserInput {
	private static final int MAX_POSSIBLE_CARDS = 21;
	private final String[][] playersInfo;
	private final Scanner input;
	private String[] inputTokens;
	private int players;

	ProcessUserInput(InputStream in) {
		input = new Scanner(in);
		findNumberOfPlayers();
		playersInfo = new String[players][MAX_POSSIBLE_CARDS];
		printInstructions();
		createMatrixFromInput();
		letUserVerifyData();
	}

	private void findNumberOfPlayers() {
		System.out.println("Enter number of players: ");
		players = Integer.parseInt(input.nextLine());

		while (players < 2) {
			System.out.println("need at least 2 players, try again: ");
			players = Integer.parseInt(input.nextLine());
		}
	}

	private void printInstructions() {
		final String instructions = "Enter player name " +
				"and the cards they have.\n" +
				"use the following syntax:\n\n" +
				"Bob: Three of Hearts, Six of Spades, Seven of Clubs\n\n" +
				"Be sure to use delimiters ':' and ','\n\n";
		System.out.print(instructions);
	}

	private void createMatrixFromInput() {
		for (int i = 0; i < players; i++) {
			System.out.print("Enter player name and their cards: ");
			String userInput = input.nextLine();

			// Split at newline char "\n", ":", and ",".
			inputTokens = userInput.split("[\n:,]");

			copyTokensInto2dArray(i);
		}
	}

	private void copyTokensInto2dArray(int i) {
		for (int j = 0; j < inputTokens.length; j++) {
			playersInfo[i][j] = inputTokens[j];
		}
	}

	private void letUserVerifyData() {
		printUserData();

		System.out.print("\n" + "Is the above data correct? y/n : ");
		String userInput = input.nextLine();
		while (!userInput.contains("y")) {
			if (userInput.contains("n")) {
				findNumberOfPlayers();
				printInstructions();
				createMatrixFromInput();
				letUserVerifyData();
				break;
			} else {
				System.out.print("Invalid input, please enter y or n: ");
				userInput = input.next();
			}
		}
	}

	private void printUserData() {
		String userData = "";
		for (int row = 0; row < players; row++) {
			for (int col = 0; col < MAX_POSSIBLE_CARDS; col++) {
				String element = playersInfo[row][col];
				if (element != null) {
					userData += " " + element;
				}
			}
			userData += "\n";
		}
		System.out.println("\nData received:");
		System.out.print(userData);
	}

	public int getNumberOfPlayers() {
		return players;
	}

	public String[][] getPlayersInfo() {
		return playersInfo;
	}
}
