import org.junit.Test;

import static org.junit.Assert.*;

public class HuffmanCodingTest {
    @Test
    public void invalidRequestReturnsProperValue() {
        HuffmanCoding hc = new HuffmanCoding("");
        assertEquals(-1, hc.getFrequency('?'));
        assertEquals("", hc.getCodeWord('?'));
    }

    @Test
    public void canEncodeHuffmanCodes() {
        HuffmanCoding hc = new HuffmanCoding("this is an example");
        char[] c = {' ', 'a', 'e', 'h', 'i', 'l', 'm', 'n', 'p', 's', 't', 'x'};
        int[] f = {3, 2, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1};
        String[] w = {"111", "011", "001", "0000", "101", "11010", "1100", "0001", "1000", "010", "1001", "11011"};
        assertThatCharsHave(hc, c, f, w);
    }

    private void assertThatCharsHave(HuffmanCoding hc, char[] c, int[] frequency, String[] codeWord) {
        boolean lengthsMatch = c.length == frequency.length && frequency.length == codeWord.length;
        assertTrue(lengthsMatch);

        for (int i = 0; i < c.length; ++i) {
            assertEquals(frequency[i], hc.getFrequency(c[i]));
            assertEquals(codeWord[i], hc.getCodeWord(c[i]));
        }
    }
}
