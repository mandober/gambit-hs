module Parsers.CSVParser where
{-
PARSING
=======
The basic idea of a parser is that it takes an input, e.g. a string, and it consumes the characters it recognizes, then passes the rest of the string as input for the subsequent parsers in the "parsing assembly line".

This indicates that an individual parser should return a pair made up of the chars it successfully recognized and the rest of the string. Or, in case of failure, possibly a pair of empty strings as indication of error. Or better yet, it should return an Either type where the Left holds the error, and Right holds the result, i.e. a pair "(result, rest)".


CSVParser

http://book.realworldhaskell.org/read/using-parsec.html

Each line is a record, each field in the record is comma separated from the next

The first example is much longer than it needs to be. We will introduce more Parsec features later and shrink the parser down to only 4 lines.

The General Parser `GenParser tok st a` data type represents a parser that parses tokens of type `tok` with a user supplied state `st` and returns a value of type `a` on success. GenParser is an instance of Functor, Monad, MonadPlus.

"Parser a" is a type synonym for "GenParser Char () a" i.e. a parser which parses Chars, has no internal state (unit), and returns something of type a.

GenParser Char st [[String]]

[
    [[A1, A2]]
]

-}
import Text.ParserCombinators.Parsec

-- CSV file contains 0+ lines, each terminated by eol
csvFile :: GenParser Char st [[String]]
csvFile =
    do result <- many line
       eof
       return result

-- each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line =
    do result <- cells
       eol
       return result

-- build a list of cells. try parsing 1st, then figure out what ends it
cells :: GenParser Char st [String]
cells =
    do first <- cellContent
       next <- remainingCells
       return (first : next)


-- a cell ends either with a comma, indicating 1+ cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma? More cells coming
    <|> (return [])                -- No comma? Return [], no more cells


-- Each cell contains 0 or more characters, which must not be a comma or EOL
cellContent :: GenParser Char st String
cellContent =
    many (noneOf ",\n")


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
