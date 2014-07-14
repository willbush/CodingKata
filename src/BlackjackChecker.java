import java.util.Arrays;

public final class BlackjackChecker {
    private final int blackJack = 21;
    private final int numberOfPlayers;
    private final String[][] playersInfo;
    private int[] playerScores;
    private int[] playerCardQuantity;
    private boolean[] fiveCardTricks;
    private boolean winByFiveCardTrick = false;
    private boolean tiedGame = false;
    private String nameOfWinner;
    private int highScore;

    BlackjackChecker(int numberOfPlayers, String[][] playersInfo) {
        this.numberOfPlayers = numberOfPlayers;
        this.playersInfo = playersInfo;
        fiveCardTricks = new boolean[numberOfPlayers];
        playerScores = new int[numberOfPlayers];
        playerCardQuantity = new int[numberOfPlayers];
        findPlayerTotals();
        findWinner();
    }

    public enum Cards {
        KING(10, "King"),
        QUEEN(10, "Queen"),
        JACK(10, "Jack"),
        TEN(10, "Ten"),
        NINE(9, "Nine"),
        EIGHT(8, "Eight"),
        SEVEN(7, "Seven"),
        SIX(6, "Six"),
        FIVE(5, "Five"),
        FOUR(4, "Four"),
        THREE(3, "Three"),
        TWO(2, "Two");

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
        String element;
        int sum;
        int numberOfAceCards;
        int totalNumberOfCards;

        for (int row = 0; row < numberOfPlayers; row++) {
            // reset
            sum = 0;
            numberOfAceCards = 0;
            totalNumberOfCards = 0;

            // Skipping first column, which contains player names.
            for (int col = 1; col < playersInfo[1].length; col++) {
                if (playersInfo[row][col] == null) break;

                totalNumberOfCards++;

                element = playersInfo[row][col];
                if (element.contains("Ace")) {
                    numberOfAceCards++;
                } else {
                    sum += findNonAceCardValue(element);
                }
            }
            playerScores[row] = calculateScore(sum, numberOfAceCards);
            playerCardQuantity[row] = totalNumberOfCards;
        }
    }

    private int calculateScore(int sum, int numberOfAceCards) {
        final int aceUpperValue = 11;
        final int aceLowerValue = 1;
        int score = sum;

        if (sum >= blackJack && numberOfAceCards > 0) {
            score += numberOfAceCards * aceLowerValue;
        } else {
            int difference = blackJack - sum;
            while (numberOfAceCards > 0) {
                if (sum == 0) {
                    score += aceUpperValue;
                    difference = score - blackJack;
                    numberOfAceCards--;
                } else if (difference < aceUpperValue) {
                    score += numberOfAceCards * aceLowerValue;
                    difference = score - blackJack;
                    numberOfAceCards = 0;
                } else if (aceUpperValue + (numberOfAceCards - 1) > blackJack) {
                    score += numberOfAceCards * aceLowerValue;
                    numberOfAceCards = 0;
                } else {
                    score += aceUpperValue;
                    difference -= aceUpperValue;
                    numberOfAceCards--;
                }
            }
        }
        return score;
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

    private void findWinner() {
        highScore = 0;
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

        for (int i = 0; i < numberOfPlayers; i++) {
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
        for (int i = 0; i < numberOfPlayers; i++) {
            if (playerScores[i] > highScore && playerScores[i] <= blackJack) {
                highScore = playerScores[i];
                indexOfWinner = i;
            }
        }
        return indexOfWinner;
    }

    private boolean playerHasFiveCardTrick(int i) {
        return playerCardQuantity[i] == 5 && playerScores[i] <= blackJack;
    }

    private boolean highScoresAreTied() {
        boolean highScoresAreTied = false;
        int[] testArray = Arrays.copyOf(playerScores, playerScores.length);
        // Sort to help find tied scores
        // sorted from least to greatest
        Arrays.sort(testArray);
        int index = testArray.length - 1;
        // greatest entry might be bust so reverse search
        // for entries not higher than blackJack before
        // checking for tied scores.
        while (testArray[index] > blackJack && index > 0) {
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
        if (tiedGame && winByFiveCardTrick) {
            System.out.println("Game Tied with two 5-card tricks");
        } else if (tiedGame) {
            System.out.println("Game Tided with two high scores");
        } else if (winByFiveCardTrick) {
            System.out.println(nameOfWinner + " won by 5-card trick " +
                    "with a score of " + highScore);
        } else if (highScore == blackJack) {
            System.out.println(nameOfWinner + " won by BlackJack." +
                    " Score: " + highScore);
        }
    }

    public int[] getPlayerScores() {
        return playerScores;
    }

    public boolean getWinByFiveCardTrick() {
        return winByFiveCardTrick;
    }

    public boolean getTiedGame() {
        return tiedGame;
    }

    public static void main(String[] args) {
        ProcessUserInput input = new ProcessUserInput(System.in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
    }

}

