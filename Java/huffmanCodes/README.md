#Huffman Codes

This is a Java implementation of Huffman encoding as described in Introduction to Algorithms 3rd edition (aka CLRS) in section 16.3.

Example input:
```
➜  huffmanCodes git:(master) java HuffmanCoding "this is an example"
```

Example output:
```
Character   Frequency   CodeWord
            3           111
a           2           011
e           2           001
h           1           0000
i           2           101
l           1           11010
m           1           1100
n           1           0001
p           1           1000
s           2           010
t           1           1001
x           1           11011
```

The implementation is written to be testable from a test harness without depending on specific output printed to System.out.
