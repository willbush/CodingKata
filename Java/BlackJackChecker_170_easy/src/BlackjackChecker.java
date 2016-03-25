import java.util.Arrays;

public final class BlackjackChecker {
	private static final int BLACKJACK = 21;
	private static final int ACE_UPPER = 11;
	private static final int ACE_LOWER = 1;
	private final int players;
	private final String[][] playersInfo;
	private final int[] playerScores;
	private final int[] playerCardQuantity;
	private final boolean[] fiveCardTricks;
	private boolean winByFiveCardTrick = false;
	private boolean tiedGame = false;
	private String nameOfWinner;
	private int highScore;

	BlackjackChecker(int players, String[][] playersInfo) {
		this.players = players;
		this.playersInfo = playersInfo;
		fiveCardTricks = new boolean[players];
		playerScores = new int[players];
		playerCardQuantity = new int[players];
		findPlayerTotals();
		findWinner();
	}

	public enum Cards {
		KING(10, "King"), QUEEN(10, "Queen"), JACK(10, "Jack"), TEN(10, "Ten"),
		NINE(9, "Nine"), EIGHT(8, "Eight"), SEVEN(7, "Seven"), SIX(6, "Six"),
		FIVE(5, "Five"), FOUR(4, "Four"), THREE(3, "Three"), TWO(2, "Two");

		private int value;
		private String name;

		private Cards(int value, String name) {
			this.value = value;
			this.name = name;
		}

		public int getValue() {
			return value;
		}

		public String getName() {
			return name;
		}
	}

	private void findPlayerTotals() {
		for (int row = 0; row < players; row++) {
			// reset
			int sum = 0, aceCards = 0, totalCards = 0;

			// Skipping first column, which contains player names.
			for (int col = 1; col < playersInfo[1].length; col++) {
				if (playersInfo[row][col] == null) break;

				totalCards++;

				String element = playersInfo[row][col];
				if (element.contains("Ace")) {
					aceCards++;
				} else {
					sum += findNonAceCardValue(element);
				}
			}
			playerScores[row] = calculateScore(sum, aceCards);
			playerCardQuantity[row] = totalCards;
		}
	}

	private int findNonAceCardValue(String element) {
		int cardValue = 0;
		for (Cards c : Cards.values()) {
			if (element.contains(c.getName())) {
				cardValue = c.getValue();
			}
		}
		return cardValue;
	}

	private int calculateScore(int sum, int aceCards) {
		int score = sum;

		if (sum >= BLACKJACK && aceCards > 0) {
			score += aceCards * ACE_LOWER;
		} else {
			score = tryToAddAceCardsWithoutBusting(aceCards, sum);
		}
		return score;
	}

	private int tryToAddAceCardsWithoutBusting(final int aceCards, int sum) {
		int score = sum;
		int myAceCards = aceCards;
		int delta;
		while (myAceCards > 0) {
			delta = BLACKJACK - score;
			if (score == 0) {
				score += ACE_UPPER;
				myAceCards--;
			} else if (anUpperValueAceWillBust(delta, myAceCards)) {
				score += myAceCards * ACE_LOWER;
				myAceCards = 0;
			} else {
				score += ACE_UPPER;
				myAceCards--;
			}
		}
		return score;
	}

	private boolean anUpperValueAceWillBust(int delta, int aceCards) {
		return delta < ACE_UPPER + (aceCards - 1);
	}

	private void findWinner() {
		int indexOfWinner = -1;
		int playersWith5cardTrick = checkFiveCardTricks();

		if (playersWith5cardTrick > 1) {
			winByFiveCardTrick = true;
			tiedGame = true;
		} else if (playersWith5cardTrick == 1) {
			winByFiveCardTrick = true;
			indexOfWinner = get5CardTrickWinnerIndex(indexOfWinner);
		} else if (highScoresAreTied()) {
			tiedGame = true;
		} else {
			indexOfWinner = getHighScoreWinnerIndex(indexOfWinner);
		}

		if (indexOfWinner > -1) {
			nameOfWinner = playersInfo[indexOfWinner][0];
		}
	}

	private int checkFiveCardTricks() {
		int playersWith5cardTrick = 0;

		for (int i = 0; i < players; i++) {
			if (playerHasFiveCardTrick(i)) {
				fiveCardTricks[i] = true;
				playersWith5cardTrick++;
				highScore = playerScores[i];
			}
		}
		return playersWith5cardTrick;
	}

	private int get5CardTrickWinnerIndex(int indexOfWinner) {
		for (int i = 0; i < fiveCardTricks.length; i++) {
			if (fiveCardTricks[i]) {
				indexOfWinner = i;
			}
		}
		return indexOfWinner;
	}

	private int getHighScoreWinnerIndex(int indexOfWinner) {
		for (int i = 0; i < players; i++) {
			if (playerScores[i] > highScore && playerScores[i] <= BLACKJACK) {
				highScore = playerScores[i];
				indexOfWinner = i;
			}
		}
		return indexOfWinner;
	}

	private boolean playerHasFiveCardTrick(int i) {
		return playerCardQuantity[i] == 5 && playerScores[i] <= BLACKJACK;
	}

	/*
	This method sorts a copy of playerScores to help find tied games.
	Since high score entries might be a bust (higher than 21),
	the method reverse searches for entries not higher than 21 (Blackjack)
	before checking for a tie.
	 */
	private boolean highScoresAreTied() {
		boolean highScoresAreTied = false;
		int[] testArray = Arrays.copyOf(playerScores, playerScores.length);
		Arrays.sort(testArray);
		int index = testArray.length - 1;
		while (testArray[index] > BLACKJACK && index > 0) {
			index--;
		}
		if (testArray[index] == testArray[index - 1]) {
			highScoresAreTied = true;
		}
		return highScoresAreTied;
	}

	public void printResults() {
		System.out.println("\nResults array: " +
				Arrays.toString(playerScores));

		String results;
		if (tiedGame && winByFiveCardTrick) {
			results = "Game Tied with two 5-card tricks\n";
		} else if (tiedGame) {
			results = "Game Tied with more than one high score\n";
		} else if (winByFiveCardTrick) {
			results = nameOfWinner + " won by 5-card trick" +
					"with a score of " + highScore + "\n";
		} else if (highScore == BLACKJACK) {
			results = nameOfWinner + " won with a BlackJack." +
					"Score: " + highScore + "\n";
		} else {
			results = nameOfWinner + " won with a high score of " +
					highScore + "\n";
		}
		System.out.print(results);
	}

	public int[] getPlayerScores() {
		return playerScores;
	}

	public String getNameOfWinner() {
		return nameOfWinner;
	}

	public int getHighScore() {
		return highScore;
	}

	public boolean getWinByFiveCardTrick() {
		return winByFiveCardTrick;
	}

	public boolean getTiedGame() {
		return tiedGame;
	}

	public static void main(String[] args) {
		ProcessUserInput input = new ProcessUserInput(System.in);
		final int players = input.getNumberOfPlayers();
		final String[][] info = input.getPlayersInfo();
		BlackjackChecker bc = new BlackjackChecker(players, info);
		bc.printResults();
	}

}

