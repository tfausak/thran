:warning: Thran was an experiment to learn about PureScript's core functional representation ("corefn") and compile it into Haskell. Because Haskell is statically typed but PureScript's corefn isn't, the compilation is harder than it should be. If you're interested in compiling PureScript into Haskell, I ([@tfausak](https://github.com/tfausak)) would recommend hacking on [the PureScript package](https://hackage.haskell.org/package/purescript) instead. In particular, writing a function to translate `Language.PureScript.AST.Declarations.Module` into `Language.Haskell.Exts.Syntax.Module` would be a good start.

# Thran

Thran compiles PureScript into Haskell.
It is written in PureScript.

[![Thran Golem](https://i.imgur.com/iwbcQjm.jpg)](http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=423539)

To use Thran, first compile a PureScript module with `--dump-corefn`.
Then convert the corefn into Haskell.

``` shell
$ yarn install
$ yarn run purs -- compile --dump-corefn Example.purs
$ yarn run --silent thran output/Example/corefn.json > Example.hs
$ stack Example.hs
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
  - The implementation of this on the Haskell side of things is gross and requires the `AllowAmbiguousTypes` extension. See <https://github.com/tfausak/thran/issues/1>.
- Partial functions
- Mutually recursive declarations
- Data constructors
  - The implementation of this in Haskell is currently broken when a single type has multiple constructors with different numbers of arguments. See <https://github.com/tfausak/thran/issues/2>.

Currently Thran does not support (see [the to do section](#to-do)):

- Array binders like `f [x] = x`
- Record binders like `f { x } = x`
- Constructor binders like `f (Just x) = x`
- Guard clauses like `f x | true = x`
- Module imports
- Foreign imports

Thran has a few limitations based on the corefn:

- Type information isn't available, so everything has to be inferred
- Type classes, newtypes, and data types are compiled into records and functions

Thran is a proof of concept at this point.
Don't use it for anything serious.
If you want to work with PureScript's corefn in PureScript, consider using <https://github.com/paulyoung/purescript-corefn>.

## Reference

This table shows how Thran compiles PureScript declarations and expressions into Haskell.

Thing | PureScript | Haskell
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
unit | `data UnitT = UnitC` | `_UnitC = ()`
adt | <code>data T = A &#x7c; B</code> | `_A = (); _B = ()`
data | `data Tuple a b = Tuple a b` | `_Tuple = (\ x y -> (x, y))`

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
partial :: Partial => Int -> Int
partial 0 = 0

-- named pattern
named :: forall a. a -> a
named x = case x of
  y@_ -> y

-- mutually recursive declarations
mutualA :: forall a b. a -> b
mutualA x = mutualB x

mutualB :: forall a b. a -> b
mutualB x = mutualA x

-- data without constructors are not present in corefn
data Void

-- data, one constructor
data UnitT = UnitC

-- using a constructor
unit :: UnitT
unit = UnitC

-- data, multiple constructors
data Toggle = Off | On

-- data with arguments
data T a = C a

-- using a constructor with arguments
intT :: T Int
intT = C 0

-- constructor with multiple arguments
data Tuple a b = Tuple a b

-- using a constructor with multiple arguments
tuple :: forall a b. a -> b -> Tuple a b
tuple x y = Tuple x y

-- adt with an argument
data Maybe a = Nothing | Just a

-- using an adt with an argument
just :: forall a. a -> Maybe a
just x = Just x

-- adt with multiple arguments
data Either a b = Left a | Right b

-- using an adt with multiple arguments
right :: forall a b. b -> Either a b
right x = Right x

-- recursive adt
data List a = Nil | Cons a (List a)

-- type operators are not present in the corefn
infixr 6 Cons as :

-- using a recursive adt
numbers :: List Int
numbers = 1 : 2 : Nil

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

-- dummy `discard` definition to avoid pulling in the prelude
discard :: forall a b. a -> (a -> b) -> b
discard = bind
```

Thran generates this Haskell module:

``` haskell
-- stack --resolver lts-8.23 exec ghci --package bookkeeper-0.2.4 --package type-level-sets-0.8.0.0
-- Built with psc version 0.11.6.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Example (
  _Left,
  _Right,
  _Nil,
  _Cons,
  _Nothing,
  _Just,
  _C,
  _Tagged,
  _Off,
  _On,
  _Tuple,
  _UnitC,
  _Monoid,
  _Semigroup,
  append,
  apply,
  array,
  bind,
  boolean,
  character,
  constant,
  discard,
  empty,
  getName,
  identity,
  intT,
  integer,
  just,
  letIdentity,
  mutualA,
  mutualB,
  named,
  negate,
  negativeOne,
  nonEmpty,
  not,
  number,
  numbers,
  partial,
  perform,
  record,
  right,
  string,
  switch,
  triple,
  tuple,
  unit,
  whereIdentity,
  semigroupInt,
) where

import qualified Bookkeeper
import qualified GHC.OverloadedLabels
import qualified GHC.Prim
import qualified Prelude

_UnitC = ()

_Tuple = (\ value0 value1 -> (value0, value1))

_Off = ()

_On = ()

_Tagged = (\ x -> x)

_C = (\ value0 -> (value0))

_Nothing = ()

_Just = (\ value0 -> (value0))

_Nil = ()

_Cons = (\ value0 value1 -> (value0, value1))

_Left = (\ value0 -> (value0))

_Right = (\ value0 -> (value0))

_Semigroup = (\ append -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "append")) Bookkeeper.=: append))

_Monoid = (\ _Semigroup0 -> (\ empty -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "empty")) Bookkeeper.=: empty Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "Semigroup0")) Bookkeeper.=: _Semigroup0)))

unit = Example._UnitC

tuple = (\ x -> (\ y -> ((Example._Tuple x) y)))

switch = (\ x -> (case (x, x) of { (0, 0) -> 0; (1, z) -> z; (y, 1) -> y; (_, _) -> x }))

string = "thran"

semigroupInt = (Example._Semigroup (\ v -> (\ v1 -> 0)))

right = (\ x -> (Example._Right x))

record = (Bookkeeper.emptyBook)

partial = (\ dictPartial -> (\ v -> (let { __unused = (\ dictPartial1 -> (\ _Dollar_6 -> _Dollar_6)) } in ((__unused dictPartial) (case (v) of { (0) -> 0 })))))

numbers = ((Example._Cons 1) ((Example._Cons 2) Example._Nil))

number = 1.2

not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (_) -> Prelude.True }))

nonEmpty = (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) Bookkeeper.=: "thran")

negate = (\ x -> x)

negativeOne = (Example.negate 1)

named = (\ x -> (case (x) of { (y@_) -> y }))

mutualA = (\ x -> (Example.mutualB x))

mutualB = (\ x -> (Example.mutualA x))

just = (\ x -> (Example._Just x))

integer = 7

intT = (Example._C 0)

identity = (\ x -> x)

letIdentity = (let { g = Example.identity; f = Example.identity } in (g f))

whereIdentity = (let { g = Example.identity; f = Example.identity } in (g f))

getName = (\ person -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "name")) person))

empty = (\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# "empty")) dict))

constant = (\ x -> (\ y -> x))

character = 't'

boolean = Prelude.True

bind = (\ x -> (\ f -> (f x)))

discard = Example.bind

perform = (\ effect -> (\ query -> ((Example.discard effect) (\ __unused -> ((Example.bind query) (\ v -> (case (v) of { (result) -> ((Example.discard effect) (\ __unused -> ((Example.bind query) (\ v1 -> result)))) })))))))

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
-- record binding/punning
pun { joke } = joke

-- TODO: new expresison type
-- array binder
useless :: forall a. Array a -> Array a
useless xs = case xs of
  [x] -> [x]
  _ -> xs

-- TODO
-- constructor binding
isJust :: forall a. Maybe a -> Boolean
isJust x = case x of
  Nothing -> false
  Just _ -> true

-- TODO: doesn't generate anything
-- module imports
import Prelude

-- TODO: doesn't generate anything
-- foreign imports
foreign import CONSOLE :: *
```
