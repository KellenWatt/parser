module Text.Parser where

import Data.Char (isSpace, isDigit)
import qualified Data.Char as C (isSymbol, isPunctuation)
import Data.List (isPrefixOf)
import Data.Either (isLeft)
import Control.Applicative (liftA2)
import Text.Parser.Generator
import Text.Parser.Token

-- `isWhitespace xs` returns True if xs contians only whitespace characters.
isWhitespace :: String -> Bool
isWhitespace [] = False
isWhitespace xs = and $ map isSpace xs

-- `isInteger xs` returns True if xs contains only decimal digits, optionally
-- preceded by a negative sign ('-').
isInteger :: String -> Bool
isInteger [] = False
isInteger ('-':[]) = False
isInteger (x:xs) =  and $ (isDigit x || x == '-') : (map isDigit xs)

-- `isInteger xs` return True if a token (or up to 3) matches a standard 
-- floating-point number, optionally preceded by a minus sign ('-'). Note: 
-- the stream ["-", ".", "<some number>"] will not match. 
isFloat :: String -> Bool
isFloat [] = False
isFloat xs
    | xs == "." = False
    | isInteger xs = True
    | otherwise = (length whole == 0 || isInteger whole) && (length dec == 0 || (and $ map isDigit dec))
    where (whole,decPart) = break (=='.') xs
          (_,dec) = span (=='.') decPart

-- `isNumber` is an alias of isFloat, but potentially more semantically 
-- meaningful.
isNumber :: String -> Bool
isNumber = isFloat

-- `isBoolean (t,f) xs` returns True if `xs` matches `t` or `f`, which should 
-- be the truthy and falsey values, respectively. If either `t` or `f` is empty,
-- the function will return False.
isBoolean :: (String,String) -> String -> Bool 
isBoolean ([],_) _ = False
isBoolean (_,[]) _ = False
isBoolean (t,f) xs = t == xs || f == xs 

-- `isSymbol xs` returns True if `xs` is composed only of Unicode symbols 
-- and punctuations.
isSymbol :: String -> Bool
isSymbol [] = False
isSymbol xs = and $ map (\c -> C.isSymbol c || C.isPunctuation c) xs

-- `isSymbol ks k` returns True if `k` a member of `ks`. `ks` is implied to be 
-- keywords for a given language.
isKeyword :: [String] -> String -> Bool
isKeyword _ [] = False
isKeyword ks k = k `elem` ks

-- `isChar xs` returns True if xs is exactly a single character, regardless of 
-- what the character is.
isChar :: String -> Bool
isChar (_:[]) = True
isChar _ = False

-- `matchType t tt xs` matches the exact string `t`, and gives that match a Token of tt.
matchType :: String -> Token -> [String] -> Generator
matchType t _ [] = err $ "End of file found. Expected \"" ++ t ++ "\"."
matchType t tt (x:xs) = if t == x
                           then singleton xs tt x
                           else err $ "Unexpected \"" ++ x ++ "\" found when looking for \"" ++ t ++ "\"."

-- `match t xs` matches the exact string `t`, ang gives the match a Token of "token".
-- This is the same as `matchType t genericToken xs`. This is often most useful for
-- matching literal tokens that will be discarded.
match :: String -> [String] -> Generator
match t xs = matchType t genericToken xs

-- `matchOf ts tt xs` matches exactly one element of `ts`, taking the first from 
-- the right. The match has a Token of `tt`.
matchTypeOf :: [String] -> Token -> [String] -> Generator
matchTypeOf ts _ [] = err $ "End of file found. Expected one of " ++ show ts ++ "."
matchTypeOf ts tt (x:xs) = if x `elem` ts
                              then singleton xs tt x
                              else err $ "Unexpected \"" ++ x ++ 
                                         "\" found when looking for one of " ++ 
                                         show ts ++ "."

-- `matchOf ts xs` matches exactly one element of `ts`, taking the first from 
-- the right. The match has a Token of "token".
matchOf :: [String] -> [String] -> Generator
matchOf ts xs = matchTypeOf ts genericToken xs

-- `ws xs` matches one token of whitespace, defined as being composed of 
-- Unicode space characters.
ws :: [String] -> Generator
ws [] = err "End of file found. Expected whitespace."
ws (x:xs) = if isWhitespace x
               then singleton xs whitespaceToken x
               else err $ "Unexpected \"" ++ x ++ "\" found when looking for whitespace."

-- `integer xs` matches any decimal integer, including a negative sign, if present.
integer :: [String] -> Generator
integer [] = err "End of file found. Expected an integer."
integer (x:xs) = if isInteger x
                    then singleton xs intToken x
                    else err $ "Unexpected \"" ++ x ++ "\" found when looking for an integer."

-- `float xs` matches any floating point number, including a negative sign, ir present.
-- This matches any definition of a float, including integers and numbers without 
-- a fractional or a whole portion.
float :: [String] -> Generator
float [] = err "End of file found. Expected a float."
float (x:xs) 
    | isFloat num  && sec == ["."] = singleton (drop 2 xs) floatToken num
    | isFloat part && (sec == ["."] || x == ".") = singleton (drop 1 xs) floatToken part
    | isFloat x = singleton xs floatToken x
    | otherwise = err $ "Unexpected \"" ++ x ++ "\" found when looking for a float."
    where sec = take 1 xs
          third = take 1 (drop 1 xs)
          part = concat $ [x] <> sec
          num = concat $ [part] <> third

-- number is the same as float, and the distinction is purely semantic. Using 
-- number instead of float appears to indicate that it matches both integers and 
-- floating point numbers.
number :: [String] -> Generator
number = float

-- `boolean (t,f) xs` matches a token that is equal to either `t` or `f`. 
-- `t` and `f` are implied to be the truthy and falsey literals, respectively.
-- If `either `t` or `f` isn't provided, the match will automatically fail.
boolean :: (String,String) -> [String] -> Generator
boolean ([],_) _     = err "Truthy keyword not provided when looking for a boolean."
boolean (_,[]) _     = err "Falsey keyword not provided when looking for a boolean."
boolean _ []         = err "End of file found. Expected a boolean."
boolean (t,f) (x:xs) 
    | x == f || x == t = singleton xs boolToken x
    | otherwise = err $ "Unexpected \"" ++ x ++ "\" found when looking for a boolean."

-- `symbol xs` matches a token that is a sequence of Unicode symbol or 
-- punctuation characters
symbol :: [String] -> Generator
symbol [] = err "End of file found. Expected a symbol."
symbol (x:xs) = if isSymbol x 
                   then singleton xs symbolToken x
                   else err $ "Unexpected \"" ++ x ++ "\" found when looking for a symbol."

-- `identifier` matches any token that is not a number, symbol, or whitespace.
-- This function is unsafe for any language that has reserved keywords.
identifier :: [String] -> Generator
identifier [] = err "End of file found. Expected an identifier."
identifier (x:xs) 
    | not (isNumber x || isWhitespace x || isSymbol x) = singleton xs identifierToken x
    | otherwise = err $ "Unexpected \"" ++ x ++ "\" found when looking for an identifier."

-- `safeIdentifier keys xs` is a version of `identifier` that is safe for languages
-- with reserved keywords. A match will fail if the identifer is a member of `keys`.
safeIdentifier :: [String] -> [String] -> Generator
safeIdentifier _ [] = err "End of file found. Expected a keyword."
safeIdentifier keys xs@(x:_) = if not $ x `elem` keys
                                then identifier xs
                                else err $ "Unexpected \"" ++ x ++ "\" found when looking for a keyword."

-- `keyword` is a synonym of matchType and is only semantically different. 
-- Using keyword may be more intuitive than directly using matchType.
keyword :: String -> Token -> [String] -> Generator
keyword = matchType

-- `char xs` matches any token that is a single character. 
char :: [String] -> Generator
char [] = err $ "End of file found. Expected a char."
char (x:xs) = if isChar x
                 then singleton xs charToken x
                 else err $ "Unexpeced \"" ++ x ++ "\" found when looking for a char."

-- `anything xs` matchees any token. The token type is generic and holds no 
-- inherent meaning.
anything :: [String] -> Generator
anything [] = err "Unexpected end of file."
anything (x:xs) = singleton xs genericToken x

-- eol matches line endnings from all major operating systems (i.e. "\r\n" and "\n").
-- If the matched sequence it part of another token, it will be split off and the
-- remaining contents of the token will be prepended to the token stream.
eol :: [String] -> Generator
eol [] = err "End of file found. Expected line ending."
eol (x:xs)
    | "\r\n" == x || "\n" == x = singleton xs endOfLineToken x
    | "\r\n" `isPrefixOf` x = singleton ((drop 2 x):xs) endOfLineToken "\r\n"
    | '\n' == head x = singleton (tail x : xs) endOfLineToken "\n"
    | otherwise = err $ "Unexpected \"" ++ x ++ "\" found when looking for end-of-line sequence."

-- eof matches the end of input, and nothing else. That is, it will only not 
-- return an error when the input stream is empty.
eof :: [String] -> Generator
eof [] = noneGen []
eof (x:_) = err $ "End of file expected, but found \"" ++ x ++ "\"."

-- `string limit xs` matches a concatenated sequence of arbitrary tokens, 
-- surrounded by `limit`. The tokens that match `limit` are discarded.
string :: String -> [String] -> Generator
string limit [] = err $ "End of file found. Expected \"" ++ limit ++ "\"."
string limit (x:xs) 
    | x == limit = if quote /= [limit]
                      then err $ "Unexpected end of file found when looking for string delimiter \"" ++ limit ++ "\""
                      else singleton rest stringToken (concat str)
    | otherwise = err $ "Unexpected \"" ++ x ++ "\" found when looking for string delimiter \"" ++ limit ++ "\"."
    where (str,tmp) = break (==limit) xs
          (quote, rest) = span (==limit) tmp

-- Possible TODO: Provide a means of recognizing escape sequences (though 
--   that might be better left to the individual langauge)

-- Meta-functions - each of these has some special effect on parsing or 
-- returns a token type that describes a collection of related subordinate tokens.

-- `try f xs` attempts to match f, but if f is not matched, no failure is generated,
-- and execution continues as if no match was attempted.
try :: ParseFunc -> [String] -> Generator
try f xs = if isLeft ast 
              then noneGen xs
              else res
    where res@(Generator _ ast) = f xs

-- opt is the same as try, but may read better under certain circumstances, 
-- specifically where a certain token is optional (e.g. indentation) 
opt :: ParseFunc -> [String] -> Generator
opt = try

-- A common idiom is `opt ws`/`try ws`. optWS does exactly that, except it's 
-- slightly more succinct.
optWS :: [String] -> Generator
optWS xs = opt ws xs

-- `trim f xs` matches f, optionally preceded by a single whitespace token 
-- (of any size). If multiple whitespace tokens precede a match, other functions 
-- such as atLeast or multiUntil might serve your purposes better.
trim :: ParseFunc -> [String] -> Generator
trim f xs = xs #> opt ws .> f

-- `multiUntil term f xs` matches `f` multiple times in sequence up to, but not 
-- including `term` being matched.
multiUntil :: ParseFunc -> ParseFunc -> [String] -> Generator
multiUntil term f xs = foldl join base $ multiUntil' term f xs
    where base = xs `asToken` sequenceToken

-- Helper function. Not to be used directly
multiUntil' :: ParseFunc -> ParseFunc -> [String] -> [Generator]
multiUntil' term f xs 
    | eof xs == noneGen [] = [term xs]
    | otherwise = if isLeft done 
                           then gen : (multiUntil' term f rest) 
                           else []
    where (Generator _ done) = term xs
          gen@(Generator rest _) = f xs

-- Same as calling multiUntil with eof as the terminating function
multi :: ParseFunc -> [String] -> Generator
multi = multiUntil eof

-- `count n f xs` matches exactly `n` instances of `f` in sequence.
count :: Int -> ParseFunc -> [String] -> Generator
count n f xs = foldl join base $ count' n f xs
    where base = xs `asToken` sequenceToken

count' :: Int -> ParseFunc -> [String] -> [Generator]
count' n f xs
    | n < 0  = [err "Count provided to count is less than 0"]
    | n == 0 = [noneGen xs]
    | otherwise = gen : count' (n-1) f rest
    where gen@(Generator rest _) = f xs

-- `upTo n f xs` is like count, except it matches `f` at least 0 times and up to `n` 
-- times in sequence. upTo is greedy, and will consume as many tokens as it can, 
-- up to `n` matches.
upTo :: Int -> ParseFunc -> [String] -> Generator
upTo n f xs = foldl join base $ upTo' n f xs
    where base = xs `asToken` sequenceToken

upTo' :: Int -> ParseFunc -> [String] -> [Generator]
upTo' n f xs 
    | n < 0  = [err "Maximum count provided is less than 0"]
    | n == 0 = [noneGen xs]
    | otherwise = if isLeft ast
                     then [noneGen xs]
                     else gen : upTo' (n-1) f rest
    where gen@(Generator rest ast) = f xs

-- `atLeast n f xs` is like count, except it matches `f` at least `n` tokens. 
-- If there a less than `n` valid matches in sequence, atLeast will fail. 
--
-- atLeast is greedy, and will consume as many tokens as it can, until it
-- encounters an invalid token. Be careful, as this could be theoretically 
-- infinite (e.g. don't match integer against an infinite list of ints).
atLeast :: Int -> ParseFunc -> [String] -> Generator
atLeast n f xs = foldl join base $ atLeast' n f xs
    where base = xs `asToken` sequenceToken

atLeast' :: Int -> ParseFunc -> [String] -> [Generator]
atLeast' n f xs
    | null xs = if n > 0
                   then [err $ "End of file found. Minimum count not met."]
                   else [noneGen xs]
    | isLeft ast = if n < 1
                      then [noneGen xs]
                      else [err "Minimum count not met."]
    | otherwise = gen : atLeast' (n-1) f rest
    where gen@(Generator rest ast) = f xs


-- `before start f xs` is the same as `start xs .> f` (in fact, that's how it's 
-- implemented). The token(s) matched by `start` are ignored. Perhaps expressed 
-- more intuitively as an infix function in a generator chain, 
-- as in `start \`before\` f`.
before :: ParseFunc -> ParseFunc -> [String] -> Generator
before start f xs = start xs .> f

-- `after end f xs` is the same as `f xs <. end` (in fact, that's how it's 
-- implemented). The token(s) matched by `end` are ignored. Perhaps expressed 
-- more intuitively as an infix function in a generator chain, 
-- as in `end \`after\` f`. 
--
-- For example `(match ")") \`after\` integer` describes a number at the head of 
-- an ordered list (e.g. "1)")
after :: ParseFunc -> ParseFunc -> [String] -> Generator
after end f xs = f xs <. end

-- `surround start end f xs` matches an `f` surrounded by `start` and `end`.
-- The tokens matched by `start` and `end` are discarded.
surround :: ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Generator
surround start end f xs = start xs .> f <. end

-- Given some ParseFunc `f`, a common idiom is `surround start end (multiUntil end f)`. 
-- This does that, but a more succinctly. This is not the same as `surround start end (multi f)`,
-- which will always terminate in an error, unless `end` is eof.
surroundMulti :: ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Generator
surroundMulti start end f xs = start xs .> multiUntil end f <. end

-- Matches a sequence of `f` of exactly length n, surrounded by `start` and `end`
surroundCount :: ParseFunc -> ParseFunc -> ParseFunc -> Int -> [String] -> Generator
surroundCount start end f n xs = start xs .> count n f <. end

-- Similar in concept to until, excecpt every entry in a list is separated by `sep`, 
-- and the terminating condition is when the separator is not found. 
list :: ParseFunc -> ParseFunc -> [String] -> Generator
list sep f xs = foldl join base $ list' sep f xs
    where base = xs `asToken` listToken

list' :: ParseFunc -> ParseFunc -> [String] -> [Generator]
list' sep f xs = if isLeft ast 
                   then [cell] 
                   else separator : (list' sep f rest)
    where cell = f xs
          separator@(Generator rest ast) = cell <. sep

-- Probably the more common type of list is a surrounded list (e.g. [1,2,3,4]). 
-- This matches that pattern.
surroundList :: ParseFunc -> ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Generator
surroundList start sep end f xs = surround start end (list sep f) xs

-- `oneOf fs xs` matches the first ParseFunc it can of those given in `fs`, 
-- starting from the left. oneOf only fails if all of the members of fs fail
-- to match.
oneOf :: [ParseFunc] -> [String] -> Generator
oneOf [] _ = err "No parsing options provided"
oneOf fs xs = foldl (\acc f -> let gen = f xs
                                   (Generator _ res) = acc
                                   in if isLeft res then gen else acc) (err "Invalid parse") fs

-- Takes tokens until `term` is matched, and returns all tokens as a single, 
-- concatenated token. The token(s) that match `term` are left in the token stream.
-- The Token returned is generic and has no inherent meaning.
takeUntil :: ParseFunc -> [String] -> Generator
takeUntil term xs = singleton (drop n xs) genericToken $ concat (take n xs)
    where n = length $ takeUntil' term xs

takeUntil' :: ParseFunc -> [String] -> [Generator]
takeUntil' _ [] = [err "End of file found. Terminating condition not met."]
takeUntil' term xs = if isLeft done
                        then gen : (takeUntil' term $ tail xs)
                        else []
    where gen@(Generator _ done) = term xs


-- Helper functions

-- For defining some infix concept (with infix math operators being the 
-- primary intention). Returns an AST with the infix as root and left and 
-- right as children.
infixF :: ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Generator
infixF f first sec xs = Generator rest $ liftA2 (<<) (liftA2 (<<) ast fast) sast
    where (Generator xs' fast) = first xs
          (Generator xss ast) = f xs'
          (Generator rest sast) = sec xss

