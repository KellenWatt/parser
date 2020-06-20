module Text.Parser.Validator where

-- This is a legacy file. It more does validation than actuall parsing,
-- which goes directly against the principle of this project.
-- That said, I couldn't bear to just delete it, since it does work, 
-- just with a different end goal.

import Data.Char (generalCategory, isLetter, isSpace, isDigit, isAlphaNum, isPunctuation, GeneralCategory(..))
import qualified Data.Char as C
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (isNothing)

type ParseFunc = [String] -> Maybe [String]

matches :: String -> [String] -> Maybe [String]
matches _ [] = Nothing
matches text (t:ts) = if text == t then Just ts else Nothing 

isWhiteSpace :: String -> Bool
isWhiteSpace [] = False
isWhiteSpace xs = and $ map isSpace xs

ws :: [String] -> Maybe [String]
ws [] = Nothing
ws (x:xs) = if isWhiteSpace x then Just xs else Nothing

isInteger :: String -> Bool
isInteger [] = False
isInteger ('-':[]) = False
isInteger (x:xs) =  and $ (isDigit x || x == '-') : (map isDigit xs)

integer :: [String] -> Maybe [String]
integer [] = Nothing
integer (x:xs) = if isInteger x then Just xs else Nothing

isFloat :: String -> Bool
isFloat [] = False
isFloat xs 
    | isInteger xs = True
    | otherwise = (length whole == 0 || isInteger whole) && (length dec == 0 || (and $ map isDigit dec))
    where (whole,decPart) = break (=='.') xs
          (point,dec) = span (=='.') decPart

isNumber :: String -> Bool
isNumber = isFloat

float :: [String] -> Maybe [String]
float (x:d:y:xs) = if isFloat (x++d++y) then Just xs else Nothing
float _ = Nothing

isSymbol :: String -> Bool
isSymbol [] = False
isSymbol xs = and $ map (\c -> C.isSymbol c || C.isPunctuation c) xs

symbol :: [String] -> Maybe [String]
symbol [] = Nothing
symbol (x:xs) = if isSymbol x then Just xs else Nothing

-- Hardly comprehensive. Defining a specific function for your language that is 
-- more useful may be called for. (I recommend calling it ident.)
-- TODO make a keyword-safe version
identifier :: [String] -> Maybe [String]
identifier [] = Nothing
identifier (x:xs)
    | null x = Nothing
    | not (isNumber x || isWhiteSpace x || isSymbol x) = Just xs
    | otherwise = Nothing

text :: String -> [String] -> Maybe [String]
text _ [] = Nothing
text key xs = key `matches` xs

quote :: String -> [String] -> Maybe [String]
quote _ [] = Nothing
quote d (x:xs) 
    | d == x    = Just xs
    | otherwise = Nothing

string :: String -> [String] -> Maybe [String]
string _ [] = Nothing
string d xs = pure xs >>= quote d >>= rest >>= quote d
    where rest = Just . snd . (break (==d))

eol :: [String] -> Maybe [String]
eol [] = Nothing
eol (x:xs)
    | "\r\n" == x || "\n" == x = Just xs
    | "\r\n" `isPrefixOf` x = Just ((drop 2 x):xs)
    | '\n' == head x = Just ((tail x):xs)
    | otherwise = Nothing

eof :: [String] -> Maybe [String]
eof [] = Just []
eof _  = Nothing

consume :: [String] -> Maybe [String]
consume [] = Nothing
consume (x:xs) = Just xs

try :: ParseFunc -> [String] -> Maybe [String]
try f xs = if isNothing result then return xs else result
    where result = f xs

opt :: ParseFunc -> [String] -> Maybe [String]
opt = try

optWS :: [String] -> Maybe [String]
optWS xs = opt ws xs

trim :: ParseFunc -> [String] -> Maybe [String]
trim f xs = optWS xs >>= f

multiUntil :: ParseFunc -> ParseFunc -> [String] -> Maybe [String]
multiUntil term f xs = if isNothing end then f xs >>= (multiUntil term f) else return xs
    where end = term xs

multi :: ParseFunc -> [String] -> Maybe [String]
multi = multiUntil eof

surround :: ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Maybe [String]
surround start end f xs = start xs >>= f >>= end

surroundMulti :: ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Maybe [String]
surroundMulti start end f xs = start xs >>= multiUntil end f >>= end

before :: ParseFunc -> ParseFunc -> [String] -> Maybe [String]
before start f xs = start xs >>= f

after :: ParseFunc -> ParseFunc -> [String] -> Maybe [String]
after end f xs = f xs >>= end

list :: ParseFunc -> ParseFunc -> [String] -> Maybe [String]
list sep f xs = if hasSep then separator >>= (list sep f) else cell
    where cell = f xs
          separator = cell >>= sep
          hasSep = (not . isNothing) $ separator

surroundList :: ParseFunc -> ParseFunc -> ParseFunc -> ParseFunc -> [String] -> Maybe [String]
surroundList start sep end f xs = surround start end (list sep f) xs

oneOf :: [ParseFunc] -> [String] -> Maybe [String]
oneOf _ [] = Nothing
oneOf fs xs = foldl (\res f -> if isNothing res then f xs else res ) Nothing fs

takeUntil :: ParseFunc -> [String] -> Maybe [String]
takeUntil term xs = multiUntil term consume xs

-- func :: [ParseFunc] -> ParseFunc = surroundMulti
-- infix f l r


