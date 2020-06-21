module Text.Parser.Generator where

import Text.Parser.Token (Token(..), rootToken)
import Data.Either (isLeft, fromRight)
import Control.Applicative (liftA2)

-- `AST` represents a node in an Abstract Syntax Tree. Each node has a token type 
-- (Token), a token value, and potential child nodes. The only rule governing a 
-- node is that it's children must be written from left to right, though this 
-- has no inherent meaning other than as a guide for usage.
-- 
-- Using the AST constructor, a node can be in any state, and it is considered 
-- valid, even if that state is nonsensical with context. An invalid node state 
-- is represented by None, which can be used to describe an error, or describe a 
-- completely empty tree.
data AST = AST {tokenType :: Token, token :: String, children :: [AST]} 
         | None deriving (Eq, Show)

-- rootAST generates an AST that has a tokenType assigned by rootToken (whatever
-- that actual value is). This is designed to be the root of any general-case
-- parse tree (hence the name).
rootAST :: AST
rootAST = AST rootToken "" []

-- `leafAST tt t` generates an AST that is considered a "leaf" node, meaning it 
-- has no children. The created AST has a token type of `tt` and a token value 
-- of `t`. 
leafAST :: Token -> String -> AST
leafAST tt t = AST tt t []

-- `a << b` makes `b` a child of `a`, if `a` and `b` are not None. If either is 
-- None, it returns the other, unaltered. If both are None, it returns None.
(<<) :: AST -> AST -> AST
None << b = b
a << None = a
(AST tt t childs) << b = AST tt t (childs ++ [b])

-- `fromGenerator gen` is a convenience method for retrieving an AST from a 
-- Generator. If the generator is in a invalid state, it returns None.
fromGenerator :: Generator -> AST
fromGenerator gen = fromRight None (tree gen)

-- Geneartor represents an intermediate state of an parser on a series of tokens.
-- It includes the current token stream (in the form of a list) and value that 
-- represents the state of parsing. A Left value represents and contains an error, 
-- and a Right contains the Abstract Syntax Tree described by the parser so far.
data Generator = Generator {tokens :: [String], tree :: Either String AST} deriving (Eq,Show)

-- ParseFunc represents a fundamental parser combinator that takes a token 
-- stream and returns a Generator. Most often, these are the result of partially 
-- applied functions that contain some sort of matching information.
type ParseFunc = [String] -> Generator

-- `err msg` creates a Generator that is already in an invalid state, containing
-- `msg` as an error message.
err :: String -> Generator
err msg = Generator [] $ Left msg

-- `newGen xs` creates a new Generator with the token stream `xa`. The new 
-- Generator only contains a root node that has no value or inherent meaning.
newGen :: [String] -> Generator
newGen xs = Generator xs (Right rootAST)

-- `noneGen xs` creates a new Generator with the token stream `xs`. The new
-- Generator only contains None as the value of it's tree. 
--
-- This is useful for indicating the end of a recursive parsing function, as
-- it essentially disappears when combined with other generators.
noneGen :: [String] -> Generator
noneGen xs = Generator xs (Right None)

-- `singleton xs tt t` creates a new Generator with the token stream `xs`. The 
-- new Generator contains a single AST node with a token type of `tt` and a 
-- token value of `t`.
singleton :: [String] -> Token -> String -> Generator
singleton xs tt t = Generator xs (Right $ leafAST tt t)

-- `asToken xs t` bundles the token stream `xs` into a new Generator. This 
-- generator contains a single AST nod ewith a token type of `t` and an 
-- emtpy token value.
--
-- This is useful for establishing a custom token type from a custom parsing 
-- function. For example: 
--
--     pair f xs = xs \`asToken\` (Token "pair") &> f &> f
--
-- matches `f` twice in a row, and makes them both child nodes of an empty
-- AST with a type of "pair". While this isn't the most useful example, it 
-- illustrates how you can make custom nodes to return from custom parsing 
-- functions, which is often more useful than the default type of "root", 
-- which is provided by (#>).
asToken :: [String] -> Token -> Generator
asToken xs t = Generator xs (Right $ leafAST t "")

-- `as f tt xs` wraps `f` and changes the token type of the root node it 
-- returns to `tt`. This is most often useful for changing the token type of 
-- pre-made parsing functions to something more meaningful or specific.
--
-- This is similar to asToken, but is applied after-the-fact. That is, these
-- two snippets produce functionally eqivalent ParseFuncs (though the first is
-- probably slightly more efficient, given the choice)
--
--     num1 xs = xs `asToken` (Token "node") &> number
--  
--  and
--
--     num2 xs = xs #> number
--     num2 `as` (Token "node")
--
as :: ParseFunc -> Token -> [String] -> Generator
as f tt xs = if isLeft ast
                then res
                else Generator rest $ Right (AST tt t ch)
    where res@(Generator rest ast) = f xs
          Right(AST _ t ch) = ast

-- `xs #> f` wraps a token stream, `xs`, in a Generator that contains a generic 
-- root node, then attempts to match `f`, using that Generator.
(#>) :: [String] -> ParseFunc -> Generator
xs #> f = newGen xs &> f

-- `gen &> f` attempts a match, `f`, on a Generator `g` if the generator is in
-- a valid state. If it is not in a valid state, the current error is passed 
-- forward without attempting to match anything. If `f` fails and returns an 
-- invalid Generator, that error is instead kept. If the match succeeds, the 
-- new generator will contain the appropriate AST and the new token stream, with
-- matched tokens consumed.
-- 
-- As an intentional side effect of this function, you can chain ParseFuncs, and
-- each of them will become a child of the node in the first Generator. 
-- For example, given two ParseFuncs `f` and `g`, and a Generator `gen`, you can
-- chain them in the form of `gen &> f &> g`, and the ASTs produced by `f` and `g`
-- will become children of the AST in gen, and `f` and `g` will consume all the
-- tokens they need and the final generator will reflect that.
(&>) :: Generator -> ParseFunc -> Generator
gen@(Generator _ (Left _)) &> _ = gen 
(Generator ts ast) &> f = Generator after $ liftA2 (<<) ast child
    where (Generator after child) = f ts

-- `gen <. f` checks if `f` is matched, then discards any matches. This returns a 
-- Generator with the same AST as `gen`, but with a token stream reflecting `f` 
-- being matched. If `f` fails to match, then the invalid Generator returned by 
-- `f` is returned.
(<.) :: Generator -> ParseFunc -> Generator
gen@(Generator _ (Left _)) <. _ = gen
(Generator ts ast) <. f
    | isLeft child = gen
    | otherwise = Generator after ast
    where gen@(Generator after child) = f ts

-- `gen .> f` attempts to match `f`, discarding `gen` entirely. If `gen` is 
-- invalid, it will instead carry the error forward by returning `gen`.
(.>) :: Generator -> ParseFunc -> Generator
gen@(Generator _ (Left _)) .> _ = gen
(Generator ts _) .> f =  f ts

-- `join g h` takes the AST in `h` and makes it a child of the AST in `g`. The 
-- token stream of the result is the one from `g`. If either `g` or `h` is 
-- invalid, the leftmost error is returned.
--
-- This is somewhat similar to (&>), except if the ParseFunc were already 
-- applied. That said, there's no limitation on the token stream of `g` being 
-- a suffix of the one from `f`, so they can theoretically be  entirely 
-- disjoint Generators.
join :: Generator -> Generator -> Generator
join gen@(Generator _ (Left _)) _ = gen
join _ gen@(Generator _ (Left _)) = gen
join (Generator _ (Right ast)) (Generator xs (Right ast')) = Generator xs $ Right (ast << ast')

-- `parse f xs` is a convenince function for wrapping `f` in a tree with a 
-- root, then extracting the result. The resulting AST and token stream are
-- those exctracted from the Generator returned from matching `f` against `xs`.
parse :: ParseFunc -> [String] -> (Either String AST, [String])
parse f xs = (tree parsed, tokens parsed)
    where parsed = f xs

