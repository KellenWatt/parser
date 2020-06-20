# parser
A descriptive parser based on combinators

To start, if you are looking for a serious parser that does a lot more of the heavy-lifting for you, use 
something like [parsec](https://hackage.haskell.org/package/parsec). It's a lot more polished than this 
(though I'd like to believe it's less intuitive), and will almost certainly serve text parsing purposes 
signifcantly better and more efficiently.

parser presents an interface for general-case parsing, generating an Abstract Syntax Tree based on
the grammar you provide (defined by combinators). Additionally, this provides a simple interface for
creating new combinators that have custom token types. As a note, this grammar is not defined in terms 
of Backus-Naur form, but should be functionally equivalent when applied appropriately.
