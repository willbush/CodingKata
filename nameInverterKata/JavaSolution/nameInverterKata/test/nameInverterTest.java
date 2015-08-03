import org.junit.Before;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;

public class nameInverterTest {
    private NameInverter nameInverter;

    @Before
    public void setup() {
        nameInverter = new NameInverter();
    }

    @Test
    public void givenNull_returnEmptyString() {
        assertInverted(null, "");
    }

    @Test
    public void givenEmptyString_returnEmptyString() {
        assertInverted("", "");
    }

    @Test
    public void givenSimpleName_returnSimpleName() {
        assertInverted("Name", "Name");
    }

    @Test
    public void givenFirstLast_returnLastFirst() {
        assertInverted("First Last", "Last, First");
    }

    @Test
    public void givenSimpleNameWithSpaces_returnSimpleNameWithoutSpaces() {
        assertInverted(" Name ", "Name");
    }

    @Test
    public void givenFirstLastWithExtraSpaces_returnLastFirst() {
        assertInverted("  First  Last  ", "Last, First");
    }

    @Test
    public void ignoreHonorifics() {
        assertInverted("Mr. First Last", "Last, First");
        assertInverted("Mrs. First Last", "Last, First");
        assertInverted("Dr. First Last", "Last, First");
    }

    @Test
    public void postNominals_stayAtEnd() {
        assertInverted("First Last Sr.", "Last, First Sr.");
        assertInverted("First Last BS. Phd.", "Last, First BS. Phd.");
    }

    @Test
    public void integration() {
        assertInverted("  Dr.  John   Doe  III   esq.  ", "Doe, John III esq.");
    }

    private void assertInverted(String originalName, String invertedName) {
        assertEquals(invertedName, nameInverter.invertName(originalName));
    }
}
