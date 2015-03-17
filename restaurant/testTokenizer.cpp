#include <iostream>
#include <vector>
#include <sstream>
#include <string>
#include "Tokenizer.h"

using namespace std;

int main() {

    double x, y, z;

    while (1) {
        string line;

        /*
        int xx = 5;
        if (xx)
            // xx is not 0
        else
            // xx is 0
        */

        x = y = z = -1;
        cout << "Enter input (3 numbers):";
        getline(cin, line);

// 
        // parse the input line
        istringstream input(line);
        if (input >> x >> y >> z)
            cout << "input is good." << endl;
        else
            cout << "input read error." << endl;
        cout << x << " " << y << " " << z << endl;

        //one line version: if (istringstream(line) >> x >> y >> z)
        // tokenizing example
        Tokenizer str(line, " ,");
        // str.setDelimiter(" ,");
        // Tokenizer str(line, " ,");
        string token;
        while ((token = str.next()) != "")
            cout << token << endl;

        // to parse and get an int from string token
        // " 50" --> 50
        // istringstream(token) >> num;

        // identifying empty lines
        Tokenizer str2(line);
        str2.setDelimiter(" ");
        if (str2.next() == "")
            cout << "That was empty line." << endl;

    }
}
