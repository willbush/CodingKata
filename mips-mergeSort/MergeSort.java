public class MergeSort {
    private int[] list;
    private int[] subListA = new int[16];
    private int[] subListB = new int[16];
    private int subListLength = 0;

    MergeSort(int[] list) {
        this.list = list;
    }

    public void mergeSort() {
        for (int powerOf2 = 2; powerOf2 <= list.length; powerOf2 *= 2) {
            for (int i = 0; i < list.length; i += powerOf2) {
                subListLength = powerOf2 / 2;
                copyToSubList(i);
                merge(i, powerOf2);
            }
        }
    }

    private void copyToSubList(int a) {
        int b = a + subListLength;
        for (int i = 0; i < subListLength; i++) {
            subListA[i] = list[a++];
            subListB[i] = list[b++];
        }
    }

    private void merge(int index, int length) {
        int a = 0;
        int b = 0;

        int listEndPoint = index + length;
        for (int i = index; i < listEndPoint; i++) {
            boolean aHasNext = a < length / 2;
            boolean bHasNext = b < length / 2;

            if (aHasNext && bHasNext) {
                if (subListA[a] < subListB[b]) {
                    list[i] = subListA[a];
                    a++;
                } else {
                    list[i] = subListB[b];
                    b++;
                }
            } else if (aHasNext) {
                list[i] = subListA[a];
                a++;
            } else if (bHasNext) {
                list[i] = subListB[b];
                b++;
            }
        }
    }

    public int[] getList() {
        return list;
    }
}
