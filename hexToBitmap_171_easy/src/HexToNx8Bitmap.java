import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class HexToNx8Bitmap {
    public static void main(String[] args) throws IOException {
        final InputStreamReader in = new InputStreamReader(System.in);
        BufferedReader input = new BufferedReader(in);
        System.out.println("Enter hex codes delineated with spaces:");
        System.out.println("Enter an empty line to quit the program.");
        String line;
        while ((line = input.readLine()) != null && line.length() > 0) {
            printBitMap(line);
        }
    }

    private static void printBitMap(String line) {
        final String anyWhitespace = "\\s+";
        String[] inputTokens = line.trim().split(anyWhitespace);

        for (int i = 0; i < inputTokens.length; i++) {
            final int hexRadix = 16, bits = 8;
            String hexEntry = inputTokens[i];
            int entryValue = Integer.parseInt(hexEntry, hexRadix);

            for (int bitIndex = bits; bitIndex >= 0; bitIndex--) {
                if (thisBitIsNotZero(entryValue, bitIndex)) {
                    System.out.print("x");
                } else {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }

    private static boolean thisBitIsNotZero(int byteValue, int bitIndex) {
        return (byteValue & 1 << bitIndex) != 0;
    }
}
