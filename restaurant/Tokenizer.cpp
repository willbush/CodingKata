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
* CREATED: 2005-05-25
* UPDATED: 2008-01-22
*/

#include "Tokenizer.h"


Tokenizer::Tokenizer() : buffer(""), token(""), delimiter(DEFAULT_DELIMITER) {
    currPos = buffer.begin();
}

Tokenizer::Tokenizer(const std::string &str, const std::string &delimiter)
        : buffer(str), token(""), delimiter(delimiter) {
    currPos = buffer.begin();
}

Tokenizer::~Tokenizer() {
}


/*
* reset string buffer, delimiter and the cursor position
*/
void Tokenizer::set(const std::string &str, const std::string &delimiter) {
    this->buffer = str;
    this->delimiter = delimiter;
    this->currPos = buffer.begin();
}

void Tokenizer::setString(const std::string &str) {
    this->buffer = str;
    this->currPos = buffer.begin();
}

void Tokenizer::setDelimiter(const std::string &delimiter) {
    this->delimiter = delimiter;
    this->currPos = buffer.begin();
}

/*
* return the next token
* If cannot find a token anymore, return "".
*/
std::string Tokenizer::next() {
    if (buffer.size() <= 0) return ""; // skip if buffer is empty

    token.clear(); // reset token string

    this->skipLeadingDelimiters();

    appendUntilDelimiterIsMeet();
    return token;
}

void Tokenizer::appendUntilDelimiterIsMeet() {
    while (currPos != buffer.end() && !isDelimiter(*currPos)) {
        token += *currPos;
        ++currPos;
    }
}

void Tokenizer::skipLeadingDelimiters() {
    while (currPos != buffer.end() && isDelimiter(*currPos))
        ++currPos;
}

bool Tokenizer::isDelimiter(char c) {
    return (delimiter.find(c) != std::string::npos);
}
