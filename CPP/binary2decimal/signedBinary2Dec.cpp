#include <iostream>

using namespace std;

bool binaryNumIsNegative(const string &);

void incrementBinaryNum(bool *, const int);

int binaryToDecimal(const bool *, const int);

int main(const int argc, const char *argv[]) {
    const string binaryInput = argv[1];
    const int len = (int) binaryInput.length();

    bool binary[len];
    int result;

    if (binaryNumIsNegative(binaryInput)) {
        for (int i = 0; i < len; ++i)
            binary[i] = binaryInput[i] != '1'; // flip all bits

        incrementBinaryNum(binary, len);
        result = -binaryToDecimal(binary, len);
    } else {
        for (int i = 0; i < len; ++i)
            binary[i] = binaryInput[i] == '1';

        result = binaryToDecimal(binary, len);
    }
    cout << result << endl;
    return 0;
}

bool binaryNumIsNegative(const string &binary) {
    return binary[0] == '1';
}

void incrementBinaryNum(bool *binary, const int len) {
    binary[len - 1] = !binary[len - 1];
    bool hasCarryBit = !binary[len - 1];

    for (int i = len - 2; hasCarryBit && i >= 0; --i) {
        binary[i] = binary[i] ^ hasCarryBit;
        hasCarryBit = !binary[i];
    }
}

int binaryToDecimal(const bool *binary, const int len) {
    int result = 0;
    int powOfTwoMultiplier = 1; // starting with 2^0

    for (int i = len - 1; i >= 0; --i) {
        result += powOfTwoMultiplier * binary[i];
        powOfTwoMultiplier = powOfTwoMultiplier << 1;
    }
    return result;
}

