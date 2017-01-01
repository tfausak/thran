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

The compiled Haskell requires GHC 8 and [the Bookkeeper package](https://hackage.haskell.org/package/bookkeeper).

## Overview

Thran is still a young project.
So far, Thran supports:

- Module export lists
- Top-level declarations
- Array, boolean, character, function, integer, number, and string literals
- Case expressions (`case _ of _`), including `if _ then _ else _`
- Let expressions (`let _ in _`), including `_ where _`
- Do notation, but you have to bring your own `bind`
- Negative numbers, but you have to bring your own `negate`
- Records
- Record field access
- Newtypes, but they compile into functions
- Type classes, including super classes

Currently Thran does not support:

- Module imports
- Foreign imports
- Recursive declarations
- Data constructors
- Guard clauses

Thran has a few limitations based on the corefn:

- Type information isn't available, so everything has to be inferred
- Type classes, newtypes, and data types are compiled into dictionaries and functions

Thran is a proof of concept at this point.
Don't use it for anything serious.

## Reference

This table shows how Thran compiles PureScript declarations and expressions into Haskell.

| PureScript | Haskell
--- | --- | ---
boolean | `true` | `Prelude.True`
integer | `7` | `7`
number | `4.2` | `4.2`
character | `'t'` | `'t'`
string | `"thran"` | `"thran"`
array | `[1, 2]` | `[1, 2]`
function | `\ x -> x` | `(\ x -> x)`
declaration | `const x y = x` | `const = (\ x -> (\ y -> x))`
application | `f x` | `(f x)`
empty record | `{}` | `(emptyBook)`
record | `{ a: 1 }` | `(emptyBook & #a := 1)`
field | `r.f` | `(get #f r)`
case | `case x of y -> y` | `(case (x) of { (y) -> y })`
conditional | `if x then 1 else 0` | `(case (x) of { (Prelude.True) -> 1; (Prelude.False) -> 2 })`
let | `let x = 1 in x` | `(let { x = 1 } in x)`
where | `y where y = 2` | `(let { y = 2 } in y)`
newtype | `newtype T = T Int` | `_T = (\ x -> x)`
type class | `class C a where f :: a` | `_C = (\ f -> (emptyBook & #f =: f))`

## Records

Records (and therefore type classes) are more complicated than given above.
They use the [Bookkeeper](https://hackage.haskell.org/package/bookkeeper-0.2.1.1/docs/Bookkeeper.html) package.
That's where the `emptyBook` and `get` identifiers come from.
They do not actually use the [`OverloadedLabels`](https://hackage.haskell.org/package/Cabal-1.24.2.0/docs/Language-Haskell-Extension.html#v:OverloadedLabels) extension.
Instead they directly use [`GHC.OverloadedLabels.fromLabel`](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-OverloadedLabels.html#v:fromLabel)
and [`GHC.Prim.proxy#`](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#v:proxy-35-).
That allows them to use any string as a field name rather than a valid Haskell identifier.
That's a lot to unpack, so here is a complete example. Thran compiles this PureScript:

``` purescript
{ "first-name": "Thran" }."first-name"
```

Into this Haskell, minus the formatting and annotations:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

import qualified Bookkeeper
import qualified GHC.OverloadedLabels
import qualified GHC.Prim

{-
  Records are built up like this:
    -- { a: 1, b: 2 }
    emptyBook & #a =: 1 & #b =: 2

  Fields are accessed like this:
    -- x.a
    get #a x
-}

(
  Bookkeeper.get
  -- This creates a field label. Note that `#first-name` is invalid syntax.
  (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "first-name"))
  (
    Bookkeeper.emptyBook
    Bookkeeper.&
      (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "first-name"))
      Bookkeeper.=:
      "Thran"
  )
)
```

## Example

Given the following PureScript module:

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
