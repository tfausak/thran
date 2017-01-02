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

The compiled Haskell can be run with [Stack](https://docs.haskellstack.org/en/stable/README/).

## Overview

Thran is still a young project.
So far, Thran supports (see [the reference section](#reference) or [the example section](#example)):

- Module export lists
- Top-level declarations
- Array, boolean, character, function, integer, number, and string literals
- Case expressions (`case _ of _`), including `if _ then _ else _`
- Let expressions (`let _ in _`), including `_ where _`
- Do notation, but you have to bring your own `bind`
- Negative numbers, but you have to bring your own `negate`
- Records (see [the records section](#records))
- Record field access
- Newtypes
- Defining type classes, including super classes
- Partial functions
- Mutually recursive declarations

Currently Thran does not support (see [the to do section](#to-do)):

- Module imports
- Foreign imports
- Data constructors
- Guard clauses

Thran has a few limitations based on the corefn:

- Type information isn't available, so everything has to be inferred
- Type classes, newtypes, and data types are compiled into dictionaries and functions

Thran is a proof of concept at this point.
Don't use it for anything serious.

## Reference

This table shows how Thran compiles PureScript declarations and expressions into Haskell.

- | PureScript | Haskell
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
superclass | `class C <= S` | `_S = (\ x -> (emptyBook & #superclass =: x))`
named match | `\ (x@_) -> x` | `(\ v -> (case (v) of { (x@_) -> x }))`

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

The following module shows everything that Thran knows how to compile.
Given this:

``` purescript
module Example where

-- function with a single argument
identity :: forall a. a -> a
identity = \ x -> x

-- function with multiple arguments
constant :: forall a b. a -> b -> a
constant x y = x

-- function application
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

-- Boolean literal
boolean :: Boolean
boolean = true

-- Int literal
integer :: Int
integer = 7

-- Number literal
number :: Number
number = 1.2

-- Char literal
character :: Char
character = 't'

-- String literal
string :: String
string = "thran"

-- Array literal
array :: Array Int
array = [1, 2, 3]

-- empty Record literal
record :: {}
record = {}

-- non-empty Record literal
nonEmpty :: { name :: String }
nonEmpty = { name: "thran" }

-- record access
getName :: { name :: String } -> String
getName person = person.name

-- case expression
switch :: Int -> Int
switch x = case x, x of
  0, 0 -> 0
  1, z -> z
  y, 1 -> y
  _, _ -> x

-- conditional expression
not :: Boolean -> Boolean
not x = if x
  then false
  else true

-- "let ... in ..." expression
letIdentity :: forall a. a -> a
letIdentity = let
  f = identity
  g = identity
  in g f

-- "... where ..." expression
whereIdentity :: forall a. a -> a
whereIdentity = g f where
  g = identity
  f = identity

-- newtype
newtype Tagged tag value = Tagged value

-- type class
class Semigroup a where
  append :: a -> a -> a

-- super class
class Semigroup a <= Monoid a where
  empty :: a

-- type class instance
instance semigroupInt :: Semigroup Int where
  append _ _ = 0

-- operators are not present in corefn
infix 5 append as +

-- using a type class
triple :: forall a. Semigroup a => a -> a
triple x = x + x + x

-- partial function
partial 0 = 0

-- named pattern
named x = case x of
  y@_ -> y

-- mutually recursive declarations
mutualA x = mutualB x
mutualB x = mutualA x

-- data without constructors are not present in corefn
data Void

-- negative numbers
negativeOne :: Int
negativeOne = -1
-- dummy `negate` definition to avoid pulling in the prelude
negate :: forall a. a -> a
negate x = x

-- do notation
perform :: forall a b. a -> b -> b
perform effect query = do
  effect
  result <- query
  effect
  _ <- query
  result
-- dummy `bind` definition to avoid pulling in the prelude
bind :: forall a b. a -> (a -> b) -> b
bind x f = f x
```

Thran generates this Haskell module:

``` haskell
-- stack --resolver lts-7 exec ghci --package bookkeeper-0.2.4 --package type-level-sets-0.8.0.0
-- Built with psc version 0.10.3.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example (
  _Tagged,
  _Monoid,
  _Semigroup,
  append,
  apply,
  array,
  bind,
  boolean,
  character,
  constant,
  empty,
  getName,
  identity,
  integer,
  letIdentity,
  mutualA,
  mutualB,
  named,
  negate,
  negativeOne,
  nonEmpty,
  not,
  number,
  partial,
  perform,
  record,
  string,
  switch,
  triple,
  whereIdentity,
  semigroupInt,
) where

import qualified Bookkeeper
import qualified GHC.OverloadedLabels
import qualified GHC.Prim
import qualified Prelude

_Tagged = (\ x -> x)

_Semigroup = (\ append -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "append")) Bookkeeper.=: append))

_Monoid = (\ __superclass_Example__Semigroup_0 -> (\ empty -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "empty")) Bookkeeper.=: empty Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "__superclass_Example.Semigroup_0")) Bookkeeper.=: __superclass_Example__Semigroup_0)))

switch = (\ x -> (case (x, x) of { (0, 0) -> 0; (1, z) -> z; (y, 1) -> y; (_, _) -> x }))

string = "thran"

semigroupInt = (Example._Semigroup (\ v -> (\ v1 -> 0)))

record = (Bookkeeper.emptyBook)

partial = (\ dictPartial -> (\ v -> (let { __unused = (\ dictPartial1 -> (\ _Dollar_6 -> _Dollar_6)) } in ((__unused dictPartial) (case (v) of { (0) -> 0 })))))

number = 1.2

not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))

nonEmpty = (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) Bookkeeper.=: "thran")

negate = (\ x -> x)

negativeOne = (Example.negate 1)

named = (\ x -> (case (x) of { (y@_) -> y }))

mutualA = (\ x -> (Example.mutualB x))

mutualB = (\ x -> (Example.mutualA x))

integer = 7

identity = (\ x -> x)

letIdentity = (let { g = Example.identity; f = Example.identity } in (g f))

whereIdentity = (let { g = Example.identity; f = Example.identity } in (g f))

getName = (\ person -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) person))

empty = (\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "empty")) dict))

constant = (\ x -> (\ y -> x))

character = 't'

boolean = Prelude.True

bind = (\ x -> (\ f -> (f x)))

perform = (\ effect -> (\ query -> ((Example.bind effect) (\ __unused -> ((Example.bind query) (\ v -> (case (v) of { (result) -> ((Example.bind effect) (\ __unused -> ((Example.bind query) (\ v1 -> (case (v1) of { (_) -> result }))))) })))))))

array = [1, 2, 3]

apply = (\ f -> (\ x -> (f x)))

append = (\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "append")) dict))

triple = (\ dictSemigroup -> (\ x -> (((Example.append dictSemigroup) (((Example.append dictSemigroup) x) x)) x)))
```

## To do

Anything missing from the above module probably does not work.
Here are things that are known to not work:

``` purescript
-- TODO: changes shape of expression
-- guard
guard x | true = x

-- TODO: changes shape of expression
-- record punning
pun { joke } = joke

-- TODO: new expresison type
-- array binder
useless :: forall a. Array a -> Array a
useless xs = case xs of
  [x] -> [x]
  _ -> xs

-- TODO: doesn't generate anything
-- module imports
import Prelude

-- TODO: doesn't generate anything
-- foreign imports
foreign import CONSOLE :: *

-- TODO: new expression type
-- data, one constructor
data Unit = Unit
-- data, constructor with arguments
data Tuple a b = Tuple a b
-- data, algebraic data type
data Maybe a = Nothing | Just a
-- data, record data type
data Point a = Point { x :: a, y :: a }
-- data, recursive data type
data List a = Nil | Cons a (List a)
```
