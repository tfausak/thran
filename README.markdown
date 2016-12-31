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

-- function with a single argument
identity = \ x -> x

-- function with multiple arguments
constant x y = x

-- function application
apply f x = f x

-- Boolean literal
boolean = true

-- Int literal
integer = 7

-- Number literal
number = 1.2

-- Char literal
character = 't'

-- String literal
string = "thran"

-- Array literal
array = [1, 2, 3]

-- empty Record literal
empty = {}

-- non-empty Record literal
nonEmpty = { name: "thran" }

-- record access
getName person = person.name

-- case expression
switch x = case x of
  y -> y

-- conditional expression
not x = if x then false else true

-- "let ... in ..." expression
letIdentity = let f = identity in f

-- "... where ..." expression
whereIdentity = f where f = identity

-- newtype
newtype Tagged tag value = Tagged value

-- type class
class Semigroup a where
  append :: a -> a -> a
```

Thran generates this Haskell module:

``` haskell
-- Built with psc version 0.10.3.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example (
  _Tagged,
  _Semigroup,
  append,
  apply,
  array,
  boolean,
  character,
  constant,
  empty,
  getName,
  identity,
  integer,
  letIdentity,
  nonEmpty,
  not,
  number,
  string,
  switch,
  whereIdentity,
) where

import qualified Bookkeeper
import qualified GHC.OverloadedLabels
import qualified GHC.Prim
import qualified Prelude

_Tagged = (\ x -> x)

_Semigroup = (\ append -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "append")) Bookkeeper.=: append))

switch = (\ x -> (case (x) of { (y) -> y }))

string = "thran"

number = 1.2

not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))

nonEmpty = (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) Bookkeeper.=: "thran")

integer = 7

identity = (\ x -> x)

letIdentity = (let { f = Example.identity } in f)

whereIdentity = (let { f = Example.identity } in f)

getName = (\ person -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) person))

empty = (Bookkeeper.emptyBook)

constant = (\ x -> (\ y -> x))

character = 't'

boolean = Prelude.True

array = [1, 2, 3]

apply = (\ f -> (\ x -> (f x)))

append = (\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "append")) dict))
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
  - Like newtypes, they compile into functions

Currently Thran does not support:

- Module imports
- Foreign imports
- Recursive declarations
- Data constructors
- Guard clauses

Thran has a few limitations based on the corefn:

- Type information isn't available, so everything has to be inferred
- Type classes, newtypes, and data types aren't translated one-to-one
  - They translate as dictionaries and functions

Thran is a proof of concept at this point.
Don't use it for anything serious.
