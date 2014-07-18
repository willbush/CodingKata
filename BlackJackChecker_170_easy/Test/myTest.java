import org.junit.Test;

import java.io.ByteArrayInputStream;

public class myTest {
    String lowScoreWinner, blackJack, fiveCardTrick, fiveCardTrick2,
            highScoreTie, fiveCardTrickTie;

    public myTest() {
        lowScoreWinner = "3\n" +
                "Juno: Two of Hearts, Three of Spades, Four of Diamonds\n" +
                "Bob: Two of Hearts, Three of Spades\n" +
                "Chris: Two of Hearts, Three of Spades\n" +
                "y\n";

        blackJack = "3\n" +
                "Alice: Ace of Diamonds, Ten of Clubs\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Spades\n" +
                "Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs\n" +
                "y\n";

        fiveCardTrick = "4\n" +
                "Alice: Ace of Diamonds\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Spades\n" +
                "Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs\n" +
                "David: Two of Hearts, Three of Clubs, Three of Hearts," +
                " Five of Hearts, Six of Hearts\n" +
                "y\n";

        fiveCardTrick2 = "6\n" +
                "Joe: Ace of Hearts, Ace of Clubs, Six of Spades\n" +
                "Jane: Two of Hearts, Four of Clubs, Seven of Spades," +
                " Two of Clubs, Five of Hearts\n" +
                "Alice: Ace of Diamonds, King of Spades\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Spades\n" +
                "Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs\n" +
                "David: Two of Hearts, Three of Clubs, Ten of Hearts," +
                " Five of Hearts, Six of Hearts\n" +
                "y\n";

        fiveCardTrickTie = "5\n" +
                "Alice: Ace of Diamonds\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Spades\n" +
                "Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs\n" +
                "Jane: Two of Hearts, Three of Clubs, Three of Hearts," +
                " Five of Hearts, Six of Hearts\n" +
                "David: Two of Hearts, Three of Clubs, Three of Hearts," +
                " Five of Hearts, Six of Hearts\n" +
                "y\n";

        highScoreTie = "5\n" +
                "Alice: Ace of Diamonds, Nine of Clubs\n" +
                "Joe: Ace of Diamonds, Nine of Spades\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Spades\n" +
                "Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs\n" +
                "Jack: King of Clubs, Queen of Spades, Jack of Diamonds\n" +
                "y\n";
    }

    @Test
    public void canGetLowScoreWinner() {
        final byte[] bytes = lowScoreWinner.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {9, 5, 5};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals("Juno", bc.getNameOfWinner());
        org.junit.Assert.assertEquals(9, bc.getHighScore());

    }

    @Test
    public void canBlackJack() {
        final byte[] bytes = blackJack.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {21, 16, 23};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals("Alice", bc.getNameOfWinner());
        org.junit.Assert.assertEquals(21, bc.getHighScore());
    }

    @Test
    public void canFiveCardTrick() {
        final byte[] bytes = fiveCardTrick.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {11, 16, 23, 19};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals(true, bc.getWinByFiveCardTrick());
        org.junit.Assert.assertEquals("David", bc.getNameOfWinner());
        org.junit.Assert.assertEquals(19, bc.getHighScore());
    }

    @Test
    public void canFiveCardTrickTie() {
        final byte[] bytes = fiveCardTrickTie.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {11, 16, 23, 19, 19};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals(true, bc.getWinByFiveCardTrick());
        org.junit.Assert.assertEquals(true, bc.getTiedGame());
        org.junit.Assert.assertEquals(19, bc.getHighScore());
    }

    @Test
    public void canFiveCardTrick2() {
        final byte[] bytes = fiveCardTrick2.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {18, 20, 21, 16, 23, 26};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals(20, bc.getHighScore());
    }

    @Test
    public void canHighScoreTie() {
        final byte[] bytes = highScoreTie.getBytes();
        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        ProcessUserInput input = new ProcessUserInput(in, System.out);
        final int numOfPlayers = input.getNumberOfPlayers();
        final String[][] info = input.getPlayersInfo();
        BlackjackChecker bc = new BlackjackChecker(numOfPlayers, info);
        bc.printResults();
        final int[] expected = {20, 20, 16, 23, 30};
        org.junit.Assert.assertArrayEquals(expected, bc.getPlayerScores());
        org.junit.Assert.assertEquals(true, bc.getTiedGame());
    }

    @Test
    public void canEvaluateCardValue() {
        int cardValue;

        cardValue = BlackjackChecker.Cards.KING.getValue();
        org.junit.Assert.assertEquals(10, cardValue);

        cardValue = BlackjackChecker.Cards.QUEEN.getValue();
        org.junit.Assert.assertEquals(10, cardValue);

        cardValue = BlackjackChecker.Cards.JACK.getValue();
        org.junit.Assert.assertEquals(10, cardValue);

        cardValue = BlackjackChecker.Cards.TEN.getValue();
        org.junit.Assert.assertEquals(10, cardValue);

        cardValue = BlackjackChecker.Cards.NINE.getValue();
        org.junit.Assert.assertEquals(9, cardValue);

        cardValue = BlackjackChecker.Cards.EIGHT.getValue();
        org.junit.Assert.assertEquals(8, cardValue);

        cardValue = BlackjackChecker.Cards.SEVEN.getValue();
        org.junit.Assert.assertEquals(7, cardValue);

        cardValue = BlackjackChecker.Cards.SIX.getValue();
        org.junit.Assert.assertEquals(6, cardValue);

        cardValue = BlackjackChecker.Cards.FIVE.getValue();
        org.junit.Assert.assertEquals(5, cardValue);

        cardValue = BlackjackChecker.Cards.FOUR.getValue();
        org.junit.Assert.assertEquals(4, cardValue);

        cardValue = BlackjackChecker.Cards.THREE.getValue();
        org.junit.Assert.assertEquals(3, cardValue);

        cardValue = BlackjackChecker.Cards.TWO.getValue();
        org.junit.Assert.assertEquals(2, cardValue);
    }
}
