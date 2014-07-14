import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public final class ProcessUserInput {
    private final int MAX_POSSIBLE_CARDS = 21;
    private int numberOfPlayers;
    private String[][] playersInfo;
    private String[] inputTokens;
    private Scanner input;

    ProcessUserInput(InputStream in, PrintStream out) {
        input = new Scanner(in);
        findNumberOfPlayers(out);
        createMatrixFromInput(out);
        letUserVerifyData(out);
    }

    private void findNumberOfPlayers(PrintStream out) {
        out.print("Enter number of players: ");
        numberOfPlayers = Integer.parseInt(input.nextLine());

        while (numberOfPlayers < 1) {
            out.print("not enough players, try again: ");
            numberOfPlayers = Integer.parseInt(input.nextLine());
        }
    }

    private void createMatrixFromInput(PrintStream out) {
        out.println("Enter player name and card their holding.");
        out.println("E.g. (Bob: Three of Hearts, Six of Spades," +
                " Seven of Spades)");
        out.println("Be sure to use delimiters ':' and ','\n");

        playersInfo = new String[numberOfPlayers][MAX_POSSIBLE_CARDS];

        for (int i = 0; i < numberOfPlayers; i++) {
            out.print("Enter player name and their cards: ");
            String userInput = input.nextLine();

            // Split at newline char, ":", and ",".
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
        while (userInput.contains("y") == false) {
            if (userInput.contains("n")) {
                findNumberOfPlayers(out);
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
        for (int row = 0; row < numberOfPlayers; row++) {
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
        return numberOfPlayers;
    }

    public String[][] getPlayersInfo() {
        return playersInfo;
    }
}
