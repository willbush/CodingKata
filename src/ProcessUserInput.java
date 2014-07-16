import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public final class ProcessUserInput {
    private static final int MAX_POSSIBLE_CARDS = 21;
    private final String[][] playersInfo;
    private final Scanner input;
    private String[] inputTokens;
    private int players;

    ProcessUserInput(InputStream in, PrintStream out) {
        input = new Scanner(in);
        findNumberOfPlayers(out);
        playersInfo = new String[players][MAX_POSSIBLE_CARDS];
        printInstructions(out);
        createMatrixFromInput(out);
        letUserVerifyData(out);
    }

    private void findNumberOfPlayers(PrintStream out) {
        out.println("Enter number of players: ");
        players = Integer.parseInt(input.nextLine());

        while (players < 2) {
            out.println("need at least 2 players, try again: ");
            players = Integer.parseInt(input.nextLine());
        }
    }

    private void printInstructions(PrintStream out) {
        final String instructions = "Enter player name " +
                "and the cards they have.\n" +
                "use the following syntax:\n\n" +
                "Bob: Three of Hearts, Six of Spades, Seven of Clubs\n\n" +
                "Be sure to use delimiters ':' and ','\n\n";
        out.print(instructions);
    }

    private void createMatrixFromInput(PrintStream out) {
        for (int i = 0; i < players; i++) {
            out.print("Enter player name and their cards: ");
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

    private void letUserVerifyData(PrintStream out) {
        printUserData(out);

        out.print("\n" + "Is the above data correct? y/n : ");
        String userInput = input.nextLine();
        while (!userInput.contains("y")) {
            if (userInput.contains("n")) {
                findNumberOfPlayers(out);
                printInstructions(out);
                createMatrixFromInput(out);
                letUserVerifyData(out);
                break;
            } else {
                out.print("Invalid input, please enter y or n: ");
                userInput = input.next();
            }
        }
    }

    private void printUserData(PrintStream out) {
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
        out.println("\nData received:");
        out.print(userData);
    }

    public int getNumberOfPlayers() {
        return players;
    }

    public String[][] getPlayersInfo() {
        return playersInfo;
    }
}
