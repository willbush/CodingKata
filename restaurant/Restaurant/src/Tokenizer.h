/**
* Tokenizer.h
* ===========
* General purpose string tokenizer (C++ string version)
*
* The default delimiters are space(" "), tab(\t, \v), newline(\n),
* carriage return(\r), and form feed(\f).
* If you want to use different delimiters, then use setDelimiter() to override
* the delimiters. Note that the delimiter string can hold multiple characters.
*
*  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
* CREATED: 2005-05-25
* UPDATED: 2006-05-18
*/

#ifndef TOKENIZER_H
#define TOKENIZER_H

#include <string>

// default delimiter string (space, tab, newline, carriage return, form feed)
const std::string DEFAULT_DELIMITER = " \t\v\n\r\f";

class Tokenizer {
public:
    Tokenizer();

    Tokenizer(const std::string &str, const std::string &delimiter = DEFAULT_DELIMITER);

    ~Tokenizer();

    // set string and delimiter
    void set(const std::string &str, const std::string &delimiter = DEFAULT_DELIMITER);

    void setBuffer(const std::string &str);

    void setDelimiter(const std::string &delimiter);

    std::string next();


protected:


private:
    std::string buffer;
    std::string token;
    std::string delimiter;
    std::string::const_iterator currentPosition;

    void skipLeadingDelimiters();

    bool isDelimiter(char c);

    void appendUntilDelimiterIsMeet();
};

#endif
