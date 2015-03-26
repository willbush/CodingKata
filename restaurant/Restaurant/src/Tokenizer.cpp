/**
* Tokenizer.cpp
* =============
* General purpose string tokenizer (C++ string version)

* The default delimiters are space(" "), tab(\t, \v), newline(\n),
* carriage return(\r), and form feed(\f).
* If you want to use different delimiters, then use setDelimiter() to override
* the delimiters. Note that the delimiter string can hold multiple characters.
*
*  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
*/

#include "Tokenizer.h"

using namespace std;


Tokenizer::Tokenizer() : buffer(""), token(""), delimiter(DEFAULT_DELIMITER) {
    currentPosition = buffer.begin();
}

Tokenizer::Tokenizer(const string &buffer, const string &delimiter)
        : buffer(buffer), token(""), delimiter(delimiter) {
    currentPosition = buffer.begin();
}

Tokenizer::~Tokenizer() {
}

/*
* reset string buffer, delimiter and the cursor position
*/
void Tokenizer::set(const string &buffer, const string &delimiter) {
    this->buffer = buffer;
    this->delimiter = delimiter;
    currentPosition = buffer.begin();
}

void Tokenizer::setBuffer(string const &buffer) {
    this->buffer = buffer;
    currentPosition = buffer.begin();
}

void Tokenizer::setDelimiter(const string &delimiter) {
    this->delimiter = delimiter;
    currentPosition = buffer.begin();
}

/*
* return the next token
* If cannot find a token anymore, return "".
*/
string Tokenizer::next() {
    if (buffer.size() <= 0) return ""; // skip if buffer is empty

    token.clear(); // reset token string

    skipLeadingDelimiters();
    appendUntilDelimiterIsMeet();

    return token;
}

void Tokenizer::skipLeadingDelimiters() {
    while (currentPosition != buffer.end() && isDelimiter(*currentPosition))
        ++currentPosition;
}

void Tokenizer::appendUntilDelimiterIsMeet() {
    while (currentPosition != buffer.end() && !isDelimiter(*currentPosition)) {
        token += *currentPosition;
        ++currentPosition;
    }
}

bool Tokenizer::isDelimiter(char c) {
    return (delimiter.find(c) != string::npos);
}
