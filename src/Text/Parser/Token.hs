module Text.Parser.Token where

import Data.Char (generalCategory, isPunctuation, isSpace, isLetter, isAlphaNum, isDigit, toLower)
import Data.List (groupBy)

-- `Token` represents a type of token.
newtype Token = Token {tokenName :: String} deriving (Eq)

instance Show Token where
    show t = "Token \"" ++ tokenName t ++ "\""

-- Maybe move these all to submodule

-- A Token representing the root of an AST. This should rarely be used directly.
rootToken :: Token
rootToken = Token "root"

-- A Token representing a piece of text without any inherent meaning. 
-- as can be used to change the token type to somthing more specific.
genericToken :: Token
genericToken = Token "token"

-- A Token repreesenting a string literal.
stringToken :: Token
stringToken = Token "string"

-- A Token representing an integer literal. Note that this does not specify 
-- the size of the integer, just the textual value.
intToken :: Token
intToken = Token "integer"

-- A Token representing a floating point literal. Note that this does not
-- specify the precision of the float, just the textual value.
floatToken :: Token
floatToken = Token "float"

-- A Token representing a single character. This is not the numeric value of
-- the character, just the textual representation.
charToken :: Token
charToken = Token "character"

-- A Token representing a boolean literal. By default, a boolean is one of
-- a user-defined pair.
boolToken :: Token
boolToken = Token "bool"

-- A Token representing whitespace. By default, whitespace is defined as 
-- any sequence that contains only Unicode space characters.
whitespaceToken :: Token
whitespaceToken = Token "whitespace"

-- A Token representing a newline. By default, a newline is defined as "\r\n" and "\n".
endOfLineToken :: Token
endOfLineToken = Token "eol"

-- A Token representing a symbol. By default, this is any sequence of Unicode 
-- symbols or punctuation.
symbolToken :: Token
symbolToken = Token "symbol"

-- A Token representing a text identifier. By default, this any sequence of characters 
-- that is not a number, symbol, or whitespace.
identifierToken :: Token
identifierToken = Token "identifier"

-- A Token representing a text identifier that is considered a keyword. By default, this
-- is user-defined.
keywordToken :: Token
keywordToken = Token "keyword"

-- A Token representing a list. This is intended to be the root of an AST, 
-- with its children being the elements of the list.
listToken :: Token
listToken = Token "list"

-- A Token representing a generic sequence of some kind without inherent meaning. 
-- This is intended to be the root of an AST, with its children being things 
-- that are in sequence.
-- as can be used to change the token to something more meaningful.
sequenceToken :: Token
sequenceToken = Token "sequence"

-- A Token representing a function. By default, there is no definition for a 
-- function, and this Token exists as a general indicator. 
functionToken :: Token
functionToken = Token "function"

-- A Token representing a comment. In most languages, comments have no meaning,
-- but they still must be parsed. Sometimes they are discarded, while others times
-- they are processed as part of the generated AST. This exists for the latter
-- situation. By default, there is no definition for a comment.
commentToken :: Token
commentToken = Token "comment"


-- Tokenizing functions

isEnclosing :: Char -> Bool
isEnclosing c = c `elem` "({[]})\"'"

-- A general-purpose, common-sense tokenizer. Splits the input text into chunks 
-- that are probably more granular than a tokenizer designed for a specific 
-- langauge, but less granular than plain character-level lexing.
--
-- Designed with C-style languages and name specifications in mind, but should 
-- be largely applicable to most langauges. If you need more specific control 
-- (e.g. character-level or symbol-driven) over lexing (for example, for most 
-- golfing languages), this is almost certainly not useful to you.
--
-- An example of a linguistic case that would not benefit from this function is,
-- ironically, Haskell, due to it's extensive use of symbol characters, both by
-- default (e.g. ->) and as identifiers (e.g. xs'). These would be tokenized as 
-- ["-",">"] and ["xs","'"], respectively, which is both not useful and actively 
-- harmful. However, these sorts of standards are sufficiently uncommon among 
-- most modern languages that they do not merit coverage in a general-case 
-- tokenizer, and should be relegated to language-specific tokenizers.
tokenize :: String -> [String]
tokenize x = groupBy (\c d -> (generalCategory c == generalCategory d &&
                              (not (isEnclosing c) && not (isEnclosing d)) &&
                              (not (isPunctuation c) && not (isPunctuation d))) ||
                              (isSpace c && isSpace d) ||
                              ((isLetter c || c=='_') && (isAlphaNum d || d=='_')) ||
                              (c=='-' && isDigit d)) x


-- A "case-insensitive" version of tokenize. It maps everything to lower case, 
-- thus elminiating case.
caseFreeTokenize :: String -> [String]
caseFreeTokenize = tokenize . map toLower
