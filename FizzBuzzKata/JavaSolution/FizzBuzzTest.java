package FizzBuzzKata;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class FizzBuzzTest {
    private FizzBuzz fb;

    @Before
    public void FizzBuzz() {
        fb = new FizzBuzz();
    }

    @Test
    public void zeroReturnsFizzBuzz() throws Exception {
        assertEquals("FizzBuzz", fb.evaluate(0));
    }

    @Test
    public void oneReturnsOne() throws Exception {
        assertEquals("1", fb.evaluate(1));
    }

    @Test
    public void twoReturnsTwo() throws Exception {
        assertEquals("2", fb.evaluate(2));
    }

    @Test
    public void threeReturnsFizz() throws Exception {
        assertEquals("Fizz", fb.evaluate(3));
    }

    @Test
    public void fiveReturnsBuzz() throws Exception {
        assertEquals("Buzz", fb.evaluate(5));
    }

    @Test
    public void sevenReturnsSeven() throws Exception {
        assertEquals("7", fb.evaluate(7));
    }

    @Test
    public void fifteenReturnsFizzBuzz() throws Exception {
        assertEquals("FizzBuzz", fb.evaluate(15));
    }

    @Test
    public void fortyFiveReturnsFizzBuzz() throws Exception {
        assertEquals("FizzBuzz", fb.evaluate(45));
    }
}
