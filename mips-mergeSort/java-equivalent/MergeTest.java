import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;

public class MergeTest {
    @Test
    public void canSortTwo() {
        int[] list = {2, 1};
        MergeSort program = new MergeSort(list);
        program.mergeSort();
        int[] expected = {1, 2};
        assertArrayEquals(expected, program.getList());
    }

    @Test
    public void canSortFour() {
        int[] list = {4, 3, 2, 1};
        MergeSort program = new MergeSort(list);
        program.mergeSort();
        int[] expected = {1, 2, 3, 4};
        assertArrayEquals(expected, program.getList());
    }

    @Test
    public void canSortEight() {
        int[] list = {8, 7, 6, 5, 4, 3, 2, 1};
        MergeSort program = new MergeSort(list);
        program.mergeSort();
        int[] expected = {1, 2, 3, 4, 5, 6, 7, 8};
        assertArrayEquals(expected, program.getList());
    }

    @Test
    public void canSortSixteen() {
        int[] list = {16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
        MergeSort program = new MergeSort(list);
        program.mergeSort();
        int[] expected = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
        assertArrayEquals(expected, program.getList());
    }

    @Test
    public void canSortThirtyTwo() {
        int[] list = new int[32];
        int[] expected = new int[32];

        for (int i = 31; i >= 0; i--)
            list[i] = i;

        for (int i = 0; i < 32; i++)
            expected[i] = i;

        MergeSort program = new MergeSort(list);
        program.mergeSort();
        assertArrayEquals(expected, program.getList());
    }
}
