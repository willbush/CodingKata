#include <iostream>

using namespace std;

unsigned int binaryToDecimal(const bool *, const int);

int main(const int argc, const char *argv[]) {
    const string binaryInput = argv[1];
    const int len = (int) binaryInput.length();

    bool binary[len];
    for (int i = 0; i < len; ++i)
        binary[i] = binaryInput[i] == '1';

    cout << binaryToDecimal(binary, len) << endl;
    return 0;
}

unsigned int binaryToDecimal(const bool *binary, const int len) {
    unsigned int result = 0;
    unsigned int powOfTwoMultiplier = 1; // starting with 2^0 = 1

    for (int i = len - 1; i >= 0; --i) {
        result += powOfTwoMultiplier * binary[i];
        powOfTwoMultiplier = powOfTwoMultiplier << 1;
    }
    return result;
}

