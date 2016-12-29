# Thran

Thran compiles PureScript into Haskell.
It is written in PureScript.

[![Thran Golem](https://i.imgur.com/iwbcQjm.jpg)](http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=423539)

To use Thran, first compile a PureScript module with `psc --dump-corefn`.
Then convert the corefn into Haskell.

``` shell
$ psc --dump-corefn path-to/Example.purs
$ npm run thran path-to/Example/corefn.json > path-to/Example.hs
```

For example, given the following PureScript module:

``` purescript
module Example where
newtype Tagged tag value = Tagged value
class Semigroup a where
  append :: a -> a -> a
were x = y where y = x
string = "thran"
number = 1.2
not x = if x then false else true
nonEmpty = { a: 1 }
letter x = let y = x in y
integer = 7
identity x = x
empty = {}
character = 't'
case_ x = case x of
  y -> y
boolean = true
array = [1, 2, 3]
apply f x = f x
access person = person.name
```

Thran generates this Haskell module:

``` haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module Example
(_Tagged, _Semigroup, access, append, apply, array, boolean, case_, character, empty, identity, integer, letter, nonEmpty, not, number, string, were)
where
import qualified Bookkeeper
import qualified Prelude
_Tagged = (\ x -> x)
_Semigroup = (\ append -> (Bookkeeper.emptyBook Bookkeeper.& #append Bookkeeper.=: append))
were = (\ x -> (let { y = x } in y))
string = "thran"
number = 1.2
not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))
nonEmpty = (Bookkeeper.emptyBook Bookkeeper.& #a Bookkeeper.=: 1)
letter = (\ x -> (let { y = x } in y))
integer = 7
identity = (\ x -> x)
empty = (Bookkeeper.emptyBook)
character = 't'
case_ = (\ x -> (case (x) of { (y) -> y }))
boolean = Prelude.True
array = [1, 2, 3]
apply = (\ f -> (\ x -> (f x)))
append = (\ dict -> (Bookkeeper.get #append dict))
access = (\ person -> (Bookkeeper.get #name person))
```

Thran is still a young project.
So far, Thran supports:

- Module export lists
- Top-level non-recursive declarations
- Array, boolean, character, function, integer, number, and string literals
- Case expressions (`case _ of _`), including `if _ then _ else _`
- Let expressions (`let _ in _`), including `_ where _`
- Do notation, but you have to bring your own `bind`
- Negative numbers, but you have to bring your own `negate`
- Empty record literals (requires Bookkeeper)
- Non-empty record literals
- Record access
- Newtypes, but they compile into functions
  - PureScript's `newtype X = X Int` is translated into `_X = (\ x -> x)`
- Type classes, but not super classes
  - Like newtypes they compile into functions

Currently Thran does not support:

- Module imports
- Foreign imports
- Recursive declarations
- Data constructors
- Guard clauses

Thran has a few limitations based on the corefn:

- Type information isn't available, so everything has to be inferred
- Type classes, newtypes, and data types aren't translated one-to-one
  - They (will) translate as dictionaries and functions

Thran is a proof of concept at this point.
Don't use it for anything serious.
