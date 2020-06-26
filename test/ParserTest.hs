module Main (main) where

import Test.Hspec
import Test.Hspec.Runner
import Text.Parser.Generator
import Text.Parser.Token
import qualified Text.Parser as P (before, after)
import Text.Parser hiding (before, after)


import Data.Either (isLeft, fromLeft)
import Data.List (tails, permutations, intersperse)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} $ do
    describe "AST" $ do
        describe "rootAST" $ do
            it "should have root Token" $ do
                tokenType rootAST `shouldBe` rootToken
            it "should have no value" $ do
                token rootAST `shouldSatisfy` null
            it "should have no children" $ do
                children rootAST `shouldSatisfy` null

        describe "leafAST" $ do 
            -- Maybe use QuickCheck here in the future?
            let typ = (Token "type")
            let val = "value"
            let leaf = leafAST typ val

            it "should maintain token type" $ do
                tokenType leaf `shouldBe` typ
            it "should maintain value" $ do
                token leaf `shouldBe` val
            it "should have no children" $ do
                children leaf `shouldSatisfy` null
        
        describe "a << b" $ do
            -- Maybe use QuickCheck here in the future?
            let aTok = (Token "first")
                aVal = "firstValue"
                a = leafAST aTok aVal
            let bTok = (Token "second")
                bVal = "secondValue"
                b = leafAST bTok bVal
            let combined = a << b

            it "a should be same with None as second" $ do
                a << None `shouldBe` a
            it "b should be same with None as first" $ do
                None << b `shouldBe` b
            it "a should be the child of a" $ do
                combined `shouldBe` (AST aTok aVal [b])
            it "a should only have b as child" $ do
                children combined `shouldBe` [b]
            it "Repeated use should give a multiple children" $ do
                children (combined << b) `shouldBe` [b,b]
            it "b should not have any children" $ do
                map children (children combined) `shouldBe` [[]]
    
    describe "Generator" $ do
        let stream = ["hello", " ", "world"]
        describe "newGen" $ do
            let gen = newGen stream
            it "has the same token stream" $ do
                tokens gen `shouldBe` stream
            it "should be valid" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "has root as AST" $ do
                tree gen `shouldBe` (Right rootAST)

        describe "noneGen" $ do
            let gen = noneGen stream
            it "has the same token stream" $ do
                tokens gen `shouldBe` stream
            it "should be valid" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "has None as AST" $ do
                tree gen `shouldBe` (Right None)

        describe "singleton" $ do
            let tok = Token "token"
                val = "singletonValue"
                gen = singleton stream tok val
            it "has the same token stream" $ do
                tokens gen `shouldBe` stream
            it "should be valid" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "has leaf as AST" $ do
                tree gen `shouldBe` (Right (leafAST tok val))

        describe "err" $ do
            let msg = "Here there be an error"
                gen = err msg
            it "should be invalid" $ do
                tree gen `shouldSatisfy` isLeft
            it "should contain error message" $ do
                fromLeft "" (tree gen) `shouldBe` msg

        describe "fromGenerator" $ do
            let ast = AST (Token "tok") (head stream) []
                gen = Generator (tail stream) (Right ast)
                inv = err "There's an error"
            it "returns same ast" $ do
                fromGenerator gen `shouldBe` ast
            it "returns None for an invalid Generator" $ do
                fromGenerator inv `shouldBe` None

        describe "asToken" $ do
            let tok = (Token "token")
                gen = stream `asToken` tok
            it "creates a Generator with the same stream" $ do
                tokens gen `shouldBe` stream
            it "creates a valid Generator" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "creates an empty AST" $ do
                token (fromGenerator gen) `shouldBe` ""
            it "contains AST of specified type" $ do
                tokenType (fromGenerator gen) `shouldBe` tok
            it "creates leaf AST" $ do
                children (fromGenerator gen) `shouldSatisfy` null

        describe "as" $ do
            let specificToken = (Token "specific")
                f = testParseFunc stream
                g = (testParseFunc `as` specificToken) $ stream
            it "should produce valid Generator on valid input" $ do
                tree g `shouldNotSatisfy` isLeft
            it "should fail the same on invalid input" $ do
                (testParseFunc `as` specificToken) [] `shouldBe` (testParseFunc [])
            it "consumes tokens the same" $ do
                tokens f `shouldBe` tokens g
            it "should not affect node value" $ do
                token (fromGenerator f) `shouldBe` token (fromGenerator g)
            it "should have new token type" $ do
                tokenType (fromGenerator g) `shouldBe` specificToken
            it "should be different from original" $ do
                tokenType (fromGenerator g) `shouldNotBe` tokenType (fromGenerator f)

        describe "xs #> f" $ do
            let gen = stream #> testParseFunc
            it "produces new valid Generator" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "produces appropriate Generator on empty stream" $ do
                tree ([] #> testParseFunc) `shouldSatisfy` isLeft
            it "consumes tokens same as f" $ do
                tokens gen `shouldBe` tokens (testParseFunc stream)
            it "generates a tree with rootAST as parent" $ do
                (fromGenerator gen) `shouldBe` rootAST << (AST genericToken (head stream) [])
        
        describe "gen &> f" $ do
            let base = newGen stream
                gen = base &> testParseFunc
                inv = newGen [] &> testParseFunc
                leftErr = err "initial error"
            it "produces a new valid Generator" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "produces appropriate Generator on empty stream" $ do
                tree inv `shouldSatisfy` isLeft
            it "forwards invalid gen" $ do
                leftErr &> testParseFunc `shouldBe` leftErr
            it "consumes tokens same as f" $ do
                tokens gen `shouldBe` tokens (testParseFunc stream)
            it "generates a tree with gen's AST as parent" $ do
                fromGenerator gen `shouldBe` (fromGenerator base) << (AST genericToken (head stream) [])

        describe "gen <. f" $ do
            let base = newGen stream
                gen = base <. testParseFunc
                inv = newGen [] <. testParseFunc
                leftErr = err "initial error"
            it "produces a new valid Generator" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "produces appropriate Generator on empty stream" $ do
                tree inv `shouldSatisfy` isLeft
            it "forwards invalid gen" $ do
                leftErr <. testParseFunc `shouldBe` leftErr
            it "consumes tokens same as f" $ do
                tokens gen `shouldBe` tokens (testParseFunc stream)
            it "discards the tree returned by f" $ do
                fromGenerator gen `shouldBe` fromGenerator base

        describe "gen .> f" $ do
            let base = newGen stream
                gen = base .> testParseFunc
                inv = newGen [] .> testParseFunc
                leftErr = err "initial error"
            it "produces a new valid Generator" $ do
                tree gen `shouldNotSatisfy` isLeft
            it "produces appropriate Generator on empty stream" $ do
                tree inv `shouldSatisfy` isLeft
            it "forwards invalid gen" $ do
                leftErr .> testParseFunc `shouldBe` leftErr
            it "consumes tokens same as f" $ do
                tokens gen `shouldBe` tokens (testParseFunc stream)
            it "discards gen entirely" $ do
                gen `shouldBe` (testParseFunc stream)

        describe "join g h" $ do
            let gen = newGen stream
                hen = gen .> testParseFunc
                leftErr = err "on left"
                rightErr = err "on right"
                res = join gen hen
            it "produces a new valid Generator" $ do
                tree res `shouldNotSatisfy` isLeft
            it "forwards invalid g" $ do
                join leftErr hen `shouldBe` leftErr
            it "forwards invalid h" $ do
                join gen rightErr `shouldBe` rightErr
            it "makes AST in h the child of the AST in g" $ do
                fromGenerator res `shouldBe` (fromGenerator gen) << (fromGenerator hen)
            it "keeps tokens from h" $ do
                tokens res `shouldBe` tokens hen
            it "discardes tokens from g" $ do
                tokens res `shouldNotBe` tokens gen

        -- Not testing parse, as it is a helper function 
        -- (and it's just xs #> f, which is already tested)

    describe "Parser" $ do
        -- Returned token type will not be checked in any combinator tests, 
        -- for 2 reasons: 1) the token type is not integral to a function's 
        -- identity, and consequently 2) it could be changed at any time with 
        -- `as`. The exceptions to (1) are the `match` functions, as genericToken 
        -- is part of their identity that separates them from their respective 
        -- `matchType`s.
        let positives = map show ([0..1000] :: [Int])
            negatives = map show ([-1000..(-1)] :: [Int])
            ints = positives ++ negatives
            floats = map show ([(-1.1)**x | x <- [-20..20]] :: [Double]) -- generates a nicely "random" sequence
            boolOptions = ("true", "false")
            bools = ["false", "true"]
            -- All symbols on American English ASCII keyboard
            symbols = "+~!@#$%^&*()_`-=[]\\{}|:\"';,./<>?" 
            symbolS = (map (:[]) symbols) :: [String]
            multiSymbols = filter (not . null) $ tails symbols 
            -- example keywords
            keywords = ["for", "if", "int", "import", "mod", "list"]
            -- non-representative set of visible ASCII characters
            chars = map (:[]) (['a'..'z'] ++ ['A'..'Z'])
            strings = filter (not . null) $ map concat (tails chars)
            whitespaces = [" ", "\t", "\r", "\n"]
            whitespaceStrings = (map concat (permutations whitespaces)) ++ whitespaces

        let invalid = isLeft . tree
            valid = not . invalid
            allValid = and . map valid
            allInvalid = and . map invalid

        describe "Formatting tester" $ do 
            describe "isWhitespace x" $ do
                let rest = ints ++ floats ++ keywords ++ multiSymbols ++ bools ++ chars ++ strings
                it "returns False for empty String" $ do
                    isWhitespace [] `shouldBe` False
                it "returns True for valid whitespace strings" $ do
                    all isWhitespace whitespaceStrings `shouldBe` True
                it "returns False for any strings that aren't whitespace" $ do
                    any isWhitespace rest `shouldBe` False

            describe "isInteger x" $ do
                let rest = floats ++ bools ++ keywords ++ multiSymbols ++ chars ++ strings ++ whitespaceStrings
                it "returns False for empty string" $ do
                    isInteger [] `shouldBe` False
                it "returns True for valid positive integer strings" $ do
                    all isInteger positives `shouldBe` True
                it "returns True for valid negative integer strings" $ do
                    all isInteger negatives `shouldBe` True
                it "returns False for any strings that aren't integers" $ do
                    any isInteger rest `shouldBe` False

            describe "isFloat x" $ do
            -- isNumber is the same as isFloat, so these test cover that. 
            -- If that ever changes, tests will be written accordingly.
                let rest = bools ++ keywords ++ multiSymbols ++ chars ++ strings ++ whitespaceStrings
                it "returns False for empty string" $ do
                    isFloat [] `shouldBe` False
                it "returns True for valid float strings" $ do
                    all isFloat floats `shouldBe` True
                it "returns True for valid integer strings" $ do
                    all isFloat ints `shouldBe` True
                it "returns False for any strings that aren't floats" $ do
                    any isFloat rest `shouldBe` False

            describe "isBoolean (t,f) x" $ do
                let rest = ints ++ floats ++ keywords ++ multiSymbols ++ chars ++ strings ++ whitespaceStrings
                it "returns False for empty string" $ do
                    isBoolean boolOptions [] `shouldBe` False
                it "returns False if either t or f is empty" $ do
                    isBoolean ("","f") "N/A" || isBoolean ("t","") "N/A" `shouldBe` False
                it "returns True for matching t" $ do
                    isBoolean boolOptions "true" `shouldBe` True
                it "returns True for matching f" $ do
                    isBoolean boolOptions "false" `shouldBe` True
                it "returns False for any strings that aren't booleans" $ do
                    any (isBoolean boolOptions) rest `shouldBe` False

            describe "isSymbol x" $ do
                let rest = ints ++ floats ++ bools ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "returns False for empty string" $ do
                    isSymbol [] `shouldBe` False
                it "returns True for valid symbol strings" $ do
                    all isSymbol symbolS `shouldBe` True
                it "returns True for multi-character symbol strings" $ do
                    all isSymbol multiSymbols `shouldBe` True
                it "returns False for any strings that aren't symbols" $ do
                    any isSymbol rest `shouldBe` False

            describe "isKeyword keys x" $ do
                let rest = ints ++ floats ++ bools ++ multiSymbols ++ chars ++ strings ++ whitespaceStrings
                it "returns False for empty string" $ do
                    isKeyword keywords [] `shouldBe` False
                it "returns True for members of keys" $ do
                    map (isKeyword keywords) keywords `shouldSatisfy` and
                it "returns False for any strings that aren't symbols" $ do
                    any (isKeyword keywords) rest `shouldBe` False

            describe "isChar x" $ do
                let rest = filter ((/=1) . length) $ ints ++ floats ++ bools ++ keywords ++ 
                                                     multiSymbols ++ strings ++ whitespaceStrings
                    tests = symbolS ++ chars ++ whitespaces ++ (map (:[]) ['0'..'9'])
                it "returns False for empty string" $ do
                    isChar [] `shouldBe` False
                it "returns True for single character strings" $ do
                    all isChar tests `shouldBe` True
                it "returns False for any multi-character strings" $ do
                    any isChar rest `shouldBe` False

        let stream = ["hello", " ", "world"]
        describe "Primitive matching combinators" $ do
            describe "matchType token tokenType xs" $ do
                let mType = Token "match"
                    matchAgainst x = matchType x mType stream
                it "fails to match nothing" $ do
                    matchAgainst "" `shouldSatisfy` invalid
                it "matches the exact token" $ do
                    matchAgainst (head stream) `shouldNotSatisfy` invalid
                it "fails to match anything else" $ do
                    matchAgainst "something"`shouldSatisfy` invalid
                it "has the same token type as specified" $ do
                    tokenType (fromGenerator $ matchAgainst (head stream)) `shouldBe` mType
                it "has the same token as matched against" $ do
                    token (fromGenerator $ matchAgainst (head stream)) `shouldBe` (head stream)
                describe "match token xs" $ do
                    it "has generic token type" $ do
                        tokenType (fromGenerator $ match (head stream) stream) `shouldBe` genericToken
                    it "should be the same as matchType token genericToken xs" $ do
                        match (head stream) stream `shouldBe` matchType (head stream) genericToken stream
                   
            describe "matchTypeOf tokens tokenType xs" $ do
                let mType = Token "match"
                    matchAgainst xs = matchTypeOf xs mType stream
                it "fails to match nothing" $ do
                    matchAgainst [] `shouldSatisfy` invalid
                it "matches one of the tokens" $ do
                    matchAgainst stream `shouldNotSatisfy` invalid
                it "fails to match anything else" $ do
                    matchAgainst ["none", "of", "these", "are", "correct"] `shouldSatisfy` invalid
                it "has the same token type specified" $ do
                    tokenType (fromGenerator $ matchAgainst stream) `shouldBe` mType
                it "has a token that is a member of tokens" $ do
                    token (fromGenerator $ matchAgainst stream) `shouldSatisfy` (`elem` stream)
                describe "matchOf tokens xs" $ do
                    it "has generic token type" $ do
                        tokenType (fromGenerator $ matchOf stream stream) `shouldBe` genericToken
                    it "should be the same as matchTypeOf tokens genericToken xs" $ do
                        matchOf stream stream `shouldBe` matchTypeOf stream genericToken stream

            describe "ws xs" $ do
                let rest = ints ++ floats ++ keywords ++ multiSymbols ++ bools ++ chars ++ strings
                it "fails to match nothing" $ do
                    ws [] `shouldSatisfy` invalid
                it "matches whitespace" $ do
                    map (ws . (:[])) whitespaceStrings `shouldSatisfy` allValid 
                it "fails to match anything else" $ do
                    map (ws . (:[])) rest `shouldSatisfy` allInvalid

            describe "integer xs" $ do
                let rest = floats ++ bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "fails to match nothing" $ do
                    integer [] `shouldSatisfy` invalid
                it "matches positive integers" $ do
                    map (integer . (:[])) positives `shouldSatisfy` allValid
                it "matches negative integers" $ do
                    map (integer . (:[])) negatives `shouldSatisfy` allValid
                it "fails to match anything else" $ do
                    -- this includes a negative test
                    map (integer . (:[])) rest `shouldSatisfy` allInvalid
            
            describe "float xs" $ do
                let rest = bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "fails to match nothing" $ do
                    float [] `shouldSatisfy` invalid
                it "matches single-token floats" $ do
                    map (float . (:[])) floats `shouldSatisfy` allValid
                context "with double-token floats" $ do
                    it "matches decimal-last" $ do
                        float ["2348987", "."] `shouldSatisfy` valid
                    it "matches negative decimal-last" $ do
                        float ["-1234098", "."] `shouldSatisfy` valid
                    it "matches decimal-first" $ do
                        float [".", "090923"] `shouldSatisfy` valid
                    it "fails to match negative decimal-first" $ do
                        float ["-.", "098234"] `shouldSatisfy` invalid
                    it "fails to match negative decimal portion" $ do
                        float [".", "-39586"] `shouldSatisfy` invalid
                context "with triple-token floats" $ do
                    it "matches simple float" $ do
                        float ["1",".","2"] `shouldSatisfy` valid
                    it "matches negative float" $ do
                        float ["-423", ".", "9885"] `shouldSatisfy` valid
                    it "ignores decimal portion if negative" $ do
                        tokens (float ["058", "." , "-4958"]) `shouldBe` ["-4958"]
                    it "does not match only '-' as whole portion" $ do
                        float ["-", ".", "123"] `shouldSatisfy` invalid
                    it "does not consume 3 sequential numbers" $ do
                        tokens (float ["1","2","3"]) `shouldBe` ["2", "3"]
                it "fails to match anything else" $ do
                    map (float . (:[])) rest `shouldSatisfy` allInvalid
            
            describe "boolean (t,f) xs" $ do
                let rest = ints ++ floats ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "fails to match nothing" $ do
                    boolean boolOptions [] `shouldSatisfy` invalid
                it "fails if t is unspecified" $ do
                    boolean ([], "f") ["N/A"] `shouldSatisfy` invalid
                it "fails if f is unspecified" $ do 
                    boolean ("t", []) ["N/A"] `shouldSatisfy` invalid
                it "matches t" $ do
                    boolean boolOptions [(fst boolOptions)] `shouldSatisfy` valid
                it "matches f" $ do
                    boolean boolOptions [(snd boolOptions)] `shouldSatisfy` valid
                it "failed to match anything else" $ do
                    map (boolean boolOptions . (:[])) rest `shouldSatisfy` allInvalid

            describe "symbol xs" $ do
                let rest = ints ++ floats ++ bools ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "fails to match nothing" $ do
                    symbol [] `shouldSatisfy` invalid
                it "matches single-character symbols" $ do
                    map (symbol . (:[])) symbolS `shouldSatisfy` allValid
                it "matches multi-character symbols" $ do
                    map (symbol . (:[])) multiSymbols `shouldSatisfy` allValid
                it "fails to match anything else" $ do
                    map (symbol . (:[])) rest `shouldSatisfy` allInvalid

            describe "identifier xs" $ do
                let rest = ints ++ floats ++ symbolS ++ multiSymbols ++ whitespaceStrings
                    strs = chars ++ strings ++ keywords ++ bools
                    alnums = ["1hello", "world3", "inf1x"]
                it "fails to match nothing" $ do
                    identifier [] `shouldSatisfy` invalid 
                it "matches only-alpha strings" $ do
                    map (identifier . (:[])) strs `shouldSatisfy` allValid
                it "matches alphanumeric strings" $ do
                    map (identifier . (:[])) alnums `shouldSatisfy` allValid
                it "fails to match anything else" $ do
                    map (identifier . (:[])) rest `shouldSatisfy` allInvalid

            describe "safeIdentifer keys xs" $ do
                let rest = ints ++ floats ++ symbolS ++ multiSymbols ++ whitespaceStrings
                    resv = bools ++ keywords
                    test = chars ++ strings
                    alnums = ["1hello", "world3", "inf1x"]
                it "fails to match nothing" $ do
                    safeIdentifier resv [] `shouldSatisfy` invalid
                it "fails to match keywords" $ do
                    map (safeIdentifier resv . (:[])) resv `shouldSatisfy` allInvalid
                it "matches identifiers that aren't keywords" $ do
                    map (safeIdentifier resv . (:[])) (test ++ alnums) `shouldSatisfy` allValid
                it "fails to match anything else" $ do
                    map (safeIdentifier resv . (:[])) rest `shouldSatisfy` allInvalid

            describe "keyword xs" $ do
                let keyType = Token "match"
                    keyOf x = keyword x keyType stream
                it "fails to match nothing" $ do
                    keyOf "" `shouldSatisfy` invalid
                it "matches the exact token" $ do
                    keyOf (head stream) `shouldNotSatisfy` invalid
                it "fails to match anything else" $ do
                    keyOf "something"`shouldSatisfy` invalid
                it "has the same token type as specified" $ do
                    tokenType (fromGenerator $ keyOf (head stream)) `shouldBe` keyType
                it "has the same token as matched against" $ do
                    token (fromGenerator $ keyOf (head stream)) `shouldBe` (head stream)

            describe "char xs" $ do
                let rest = filter ((/=1) . length) $ ints ++ floats ++ bools ++ multiSymbols ++ keywords ++ strings ++ whitespaceStrings
                    test = chars ++ symbolS ++ (map show [(0::Int)..9])
                it "fails to match nothing" $ do
                    char [] `shouldSatisfy` invalid
                it "matches any single-character strings" $ do
                    map (char . (:[])) test `shouldSatisfy` allValid
                it "fails to match anything else" $ do
                    map (char . (:[])) rest `shouldSatisfy` allInvalid

            describe "anything xs" $ do
                let test = ints ++ floats ++ bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "fails to match nothing" $ do
                    anything [] `shouldSatisfy` invalid
                it "matches literally all possible non-empty tokens" $ do
                    map (anything . (:[])) test `shouldSatisfy` allValid

            describe "eol xs" $ do
                let rest = ints ++ floats ++ bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings
                it "fails to match nothing" $ do
                    eol [] `shouldSatisfy` invalid
                context "when end-of-line is whole token" $ do
                    it "matches \\n" $ do
                        eol ["\n"] `shouldSatisfy` valid
                    it "matches \\r\\n" $ do
                        eol ["\r\n"] `shouldSatisfy` valid
                context "when end-of-line is partial token" $ do
                    it "matches \\n" $ do
                        eol ["\n hello"] `shouldSatisfy` ((==[" hello"]) . tokens)
                    it "matches \\r\\n" $ do
                        eol ["\r\n hello"] `shouldSatisfy` ((==[" hello"]) . tokens)
                it "fails to match anything else" $ do
                    map (eol . (:[])) rest `shouldSatisfy` allInvalid

            describe "eof xs" $ do
                let rest = ints ++ floats ++ bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                it "matches nothing" $ do
                    eof [] `shouldSatisfy` valid
                it "fails to match everything else" $ do
                    map (eof . (:[])) rest `shouldSatisfy` allInvalid

            describe "string quote xs" $ do
                let quote = "\""
                    rest = ints ++ floats ++ bools ++ symbolS ++ multiSymbols ++ keywords ++ chars ++ strings ++ whitespaceStrings
                    test = map (\s -> quote : s : quote : []) . filter (/=quote) $ rest
                it "fails to match nothing" $ do
                    string quote [] `shouldSatisfy` invalid
                it "fails to match a single quote" $ do
                    string quote [quote] `shouldSatisfy` invalid
                it "matches an empty \"string\"" $ do
                    string quote [quote,quote] `shouldSatisfy` valid
                it "matches any kind of \"string\"" $ do
                    map (string quote) test `shouldSatisfy` allValid
                it "fails to match unquoted strings" $ do
                    map (string quote . (:[])) rest `shouldSatisfy` allInvalid
                it "matches \"strings\" with non-'\"' quote" $ do
                    string ":" [":", "hello\" \"world", ":"] `shouldSatisfy` valid

        describe "Modifying combinators" $ do
            describe "try f xs" $ do
                let base = newGen stream
                it "does not match nothing, but does not fail" $ do
                    try limitedParseFunc [] `shouldSatisfy` valid
                it "matches f" $ do
                    try limitedParseFunc ["3"] `shouldSatisfy` valid
                it "produces original Generator on failure" $ do
                    base &> try limitedParseFunc `shouldBe` base
                it "consumes tokens on match" $ do
                    ["4", "5"] #> try limitedParseFunc `shouldSatisfy` (==["5"]) . tokens
                describe "opt f xs" $ do
                    it "behaves the same as try" $ do
                        opt limitedParseFunc [] `shouldBe` try limitedParseFunc []
                        base &> opt limitedParseFunc `shouldBe` base &> try limitedParseFunc
                        opt limitedParseFunc ["4", "5"] `shouldBe` try limitedParseFunc ["4", "5"]
                describe "optWS xs" $ do
                    let wstest = [" ", "hello"]
                    it "behaves the same as opt ws" $ do
                        optWS [] `shouldBe` opt ws []
                        wstest #> optWS `shouldBe` wstest #> opt ws
                        tail wstest  #> optWS `shouldBe` tail wstest #> opt ws
                       
            describe "trim f xs" $ do
                let wstest = [" ", "hello", "world"]
                    nows = tail wstest
                    f = match "hello"
                it "trims leading whitespace" $ do
                    trim f wstest `shouldBe` f nows
                it "attempts to match f if no leading whitespace" $ do
                    trim f nows `shouldBe` f nows
                it "produces same result for leading or no leading whitespace" $ do
                    trim f wstest `shouldBe` trim f nows

        describe "Combinators" $ do
            -- Note that none of these functions will test validity of input, 
            -- just that they behave as expected. Validity is expected of
            -- primative combinators, modifying combinators and combining operators.
            describe "multiUntil term f xs" $ do
                let mUntil = multiUntil rightParseFunc testParseFunc
                it "fails on never finding terminator" $ do
                    mUntil stream `shouldSatisfy` invalid
                it "succeeds on immediately encountering terminator" $ do
                    mUntil [rightBound] `shouldSatisfy` valid
                it "fails on not matching f" $ do
                    multiUntil rightParseFunc limitedParseFunc ["hello", rightBound] `shouldSatisfy` invalid
                it "does not consume terminator" $ do
                    tokens (mUntil [rightBound]) `shouldBe` [rightBound]
                it "makes all matches children" $ do
                    map token ((children . fromGenerator) (mUntil (stream ++ [rightBound]))) 
                        `shouldBe` 
                        stream
                describe "multi f xs" $ do
                    it "should be same as multiUntil eof f xs" $ do
                        multi testParseFunc [] `shouldBe` multiUntil eof testParseFunc []
                        multi testParseFunc stream `shouldBe` multiUntil eof testParseFunc stream
                        multi limitedParseFunc stream `shouldBe` multiUntil eof limitedParseFunc stream
                        -- will always encounter terminator
                        -- assume it does the concatenation, add test if implementation changes

            describe "count n f xs" $ do
                it "does not accept negative inputs" $ do
                    count (-1) testParseFunc stream `shouldSatisfy` invalid
                it "accepts n of 0, returning empty sequence" $ do
                    let res = count 0 testParseFunc stream
                    res `shouldSatisfy` valid
                    tree res `shouldBe` Right (leafAST sequenceToken "")
                it "fails to match anything less than n" $ do
                    count ((length stream)+1) testParseFunc stream `shouldSatisfy` invalid
                it "matches exactly n tokens" $ do
                    let res = count ((length stream)-1) testParseFunc stream 
                    res `shouldSatisfy` valid
                    tokens res `shouldBe` [last stream]
                    (length . children . fromGenerator) res `shouldBe` ((length stream)-1)
            
            describe "upTo n f xs" $ do
                it "does not accept negative inputs" $ do
                    upTo (-1) testParseFunc stream `shouldSatisfy` invalid
                it "accepts n of 0, returning empty sequence" $ do
                    let res = upTo 0 testParseFunc stream
                    res `shouldSatisfy` valid
                    tree res `shouldBe` Right (leafAST sequenceToken "")
                it "matches less than n elements" $ do
                    let res = upTo ((length stream)+1) testParseFunc stream 
                    res `shouldSatisfy` valid
                    (length . children . fromGenerator) res `shouldBe` length stream
                it "matches at most n elements" $ do
                    let res = upTo ((length stream)-1) testParseFunc stream
                    res `shouldSatisfy` valid
                    tokens res `shouldBe` [last stream]
                    (length . children . fromGenerator) res `shouldBe` ((length stream)-1)

            describe "atLeast n f xs" $ do
                it "accepts negative n" $ do
                    let res = atLeast (-1) testParseFunc stream 
                    res `shouldSatisfy` valid
                    tokens res `shouldSatisfy` null
                it "matches at least n tokens" $ do
                    let res = atLeast ((length stream)-1) testParseFunc stream
                    res `shouldSatisfy` valid
                    tokens res `shouldSatisfy` null
                    (length . children . fromGenerator) res `shouldSatisfy` (>=(length stream))
                it "fails to match less than n tokens" $ do
                    let res = atLeast ((length stream)+1) testParseFunc stream
                    res `shouldSatisfy` invalid
                it "fails to match less than n exact tokens" $ do
                    let numStream = ["1","2","3","4","hello"]
                        res = atLeast (length numStream) limitedParseFunc numStream
                    res `shouldSatisfy` invalid

            describe "between (l,h) f xs" $ do
                it "accepts negative n" $ do
                    let res = between (-1,1) testParseFunc stream
                    res `shouldSatisfy` valid
                it "accepts h of 0, returning empty sequence" $ do
                    let res = between (-1,0) testParseFunc stream
                    tree res `shouldBe` Right (leafAST sequenceToken "")
                it "accepts l and h being equal" $ do
                    let n = (length stream) - 1
                        res = between (n,n) testParseFunc stream
                    res `shouldSatisfy` valid
                    (length . children . fromGenerator) res `shouldBe` n
                it "fails if h is less than 0" $ do
                    between (-2,-1) testParseFunc stream `shouldSatisfy` invalid
                it "fails if h is less than l" $ do
                    between (0,-1) testParseFunc stream `shouldSatisfy` invalid
                it "matches at most h tokens" $ do
                    let res = between (0, (length stream)-1) testParseFunc stream
                    res `shouldSatisfy` valid
                    (length . tokens) res `shouldBe` 1
                    (length . children . fromGenerator) res `shouldBe` (length stream) - 1

                it "matches l greater than 0" $ do
                    let res = between (1, 4) testParseFunc stream
                    res `shouldSatisfy` valid
                    (length . children .fromGenerator) res `shouldSatisfy` (>=1)
                it "fails to match less than l tokens" $ do
                    let l = (length stream) + 1
                        res = between (l, l+1) testParseFunc stream
                    res `shouldSatisfy` invalid

            describe "before start f xs" $ do
                let beforeStream = ["hello", "2", "world"]
                it "fails to match just f" $ do
                    P.before (match "hello") limitedParseFunc (tail beforeStream) `shouldSatisfy` invalid
                it "matches f when start present" $ do
                    let res = P.before (match "hello") limitedParseFunc beforeStream 
                    res `shouldSatisfy` valid
                    tokens res `shouldBe` drop 2 beforeStream
                it "discards tokens matched by start" $ do
                    let res = P.before (match "hello") limitedParseFunc beforeStream
                    (length . children . fromGenerator) res `shouldBe` 0

            describe "after end f xs" $ do
                let afterStream = ["hello", "2", "world"]
                it "fails to match just f" $ do
                    P.after (match "hello") limitedParseFunc ["2","world"] `shouldSatisfy` invalid
                it "matches f when after is present" $ do
                    let res = P.after limitedParseFunc (match "hello") afterStream 
                    res `shouldSatisfy` valid
                    tokens res `shouldBe` drop 2 afterStream
                it "discards tokens matched by end" $ do
                    let res = P.after limitedParseFunc (match "hello") afterStream
                    (length . children . fromGenerator) res `shouldBe` 0
                    
            describe "surround start end f xs" $ do
                let fab f xs = surround leftParseFunc rightParseFunc f xs
                    middle = "text"
                    surStream = [leftBound, middle, rightBound]
                it "fails to match just f" $ do
                    fab testParseFunc ["test"] `shouldSatisfy` invalid
                it "fails to match just start and f" $ do
                    fab testParseFunc (init surStream) `shouldSatisfy` invalid
                it "fails to match just f and end" $ do
                    fab testParseFunc (tail surStream) `shouldSatisfy` invalid
                it "fails to match just start and end" $ do
                    fab testParseFunc [leftBound, rightBound] `shouldSatisfy` invalid
                it "matches only when start, f, and end" $ do
                    fab testParseFunc surStream `shouldSatisfy` valid
                it "discards start and end matches" $ do
                    let res = fab testParseFunc surStream
                    (token . fromGenerator) res `shouldBe` middle
                    (length . children . fromGenerator) res `shouldBe` 0
                describe "surroundMulti start end f xs" $ do
                    let multiFab f xs = surroundMulti leftParseFunc rightParseFunc f xs
                        multiSurStream = [leftBound, middle, middle ++ "2", rightBound] 
                    it "behaves the same as surround multiUntil" $ do
                        let mult f xs= surround leftParseFunc rightParseFunc (multiUntil rightParseFunc f) xs
                        multiFab testParseFunc multiSurStream `shouldBe` mult testParseFunc multiSurStream
                describe "surroundCount start end f n xs" $ do
                    let n = 10
                        countFab f xs = surroundCount leftParseFunc rightParseFunc f n xs
                        countSurStream = [leftBound] ++ (replicate n "text") ++ [rightBound]
                    it "behaves the same as surround count" $ do
                        let coun f xs = surround leftParseFunc rightParseFunc (count n f) xs
                        countFab testParseFunc countSurStream `shouldBe` coun testParseFunc countSurStream

            describe "list sep f xs" $ do
                let sep = "."
                    sepF = (match sep)
                    text = "text"
                    lst n = intersperse sep (replicate n text)
                it "fails to match just sepaarator" $ do
                    list sepF (match text) [sep] `shouldSatisfy` invalid
                it "matches just f" $ do
                    list sepF (match text) [text] `shouldSatisfy` valid
                it "fails to match f and sep" $ do
                    list sepF (match text) [text, sep] `shouldSatisfy` invalid
                it "fails to match sep and f" $ do
                    list sepF (match text) [sep, text] `shouldSatisfy` invalid
                it "matches arbitrary length list" $ do
                    [list sepF (match text) (lst i) | i <- [1..10]] `shouldSatisfy` allValid
                describe "surroundList start sep end f xs" $ do
                    it "behaves the same as surroundList" $ do
                        let lis xs = surroundList leftParseFunc sepF rightParseFunc (match text) xs
                            comp xs = surround leftParseFunc rightParseFunc (list sepF (match text)) xs
                            l = lst 10
                        lis l `shouldBe` comp l
                    
            describe "oneOf fs xs" $ do
                let options = [integer, string "\"", ws]
                    choose = oneOf options
                it "fails if there are no options" $ do
                    oneOf [] stream `shouldSatisfy` invalid
                it "matches any of the options" $ do
                    let tests = [["4"], ["\"","here's", "text", "\""], ["\n "]] 
                    map choose tests `shouldSatisfy` allValid
                it "fails if there are no matches" $ do
                    choose ["4.5"] `shouldSatisfy` invalid

            describe "takeUntil term xs" $ do
                let term = rightParseFunc
                    takeStream = ["hello", "4", "world", "}", "\n"]
                it "takes all tokens if term is not matched" $ do
                    tokens (takeUntil term (take 3 takeStream)) `shouldSatisfy` null 
                it "should eventually terminate if term is matched"  $ do
                    takeUntil term takeStream `shouldSatisfy` valid
                it "concatenates all consumed tokens" $ do
                    let res = takeUntil term takeStream
                    (token . fromGenerator) res `shouldBe` (concat . take 3) takeStream
                it "does not consume tokens matched by term" $ do
                    let res = takeUntil term takeStream
                    (head . tokens) res `shouldBe` rightBound

            describe "infixF f start end" $ do
                let inf = "op"
                    f = match inf
                    start = limitedParseFunc
                    end = limitedParseFunc
                it "fails to match only f" $ do
                    infixF f start end [inf] `shouldSatisfy` invalid
                it "fails to match only start and f" $ do
                    infixF f start end ["5.3", inf] `shouldSatisfy` invalid
                it "fails to match only f and dend" $ do
                    infixF f start end [inf, "3"] `shouldSatisfy` invalid
                it "matches full start f end" $ do
                    infixF f start end ["6.8", inf, "4.9"] `shouldSatisfy` valid
                it "consumes all matched tokens" $ do
                    let res = infixF f start end ["3.8", inf, "2"]
                    tokens res `shouldSatisfy` null
                it "makes the f tree the parent of start and end, in order" $ do
                    let first = "3.8"
                        second = "2"
                        res = infixF f start end [first, inf, second]
                        ast = fromGenerator res
                    token ast `shouldBe` inf
                    (length . children) ast `shouldBe` 2
                    map (token) (children ast) `shouldBe` [first, second]
                    
                    

-- eventual Unicode tests


leftBound :: String
leftBound = "{"

leftParseFunc :: [String] -> Generator
leftParseFunc [] = err "no left"
leftParseFunc xs = match leftBound xs

testParseFunc :: [String] -> Generator
testParseFunc [] = err "nothing"
testParseFunc (x:xs) = singleton xs genericToken x

limitedParseFunc :: ParseFunc
limitedParseFunc = number

rightBound :: String
rightBound = "}"

rightParseFunc :: [String] -> Generator
rightParseFunc [] = err "no right"
rightParseFunc xs = match rightBound xs



