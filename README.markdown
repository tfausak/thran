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
were x = y where y = x
string = "thran"
number = 1.2
not x = if x then false else true
letter x = let y = x in y
integer = 7
identity x = x
character = 't'
case_ x = case x of
  y -> y
boolean = true
array = [1, 2, 3]
apply f x = f x
```

Thran generates this Haskell module:

``` haskell
{-# LANGUAGE NoImplicitPrelude #-}
-- Built with psc version 0.10.3.
module Example
(apply, array, boolean, case_, character, identity, integer, letter, not, number, string, were)
where
import qualified Prelude
were = (\ x -> (let { y = x } in y))
string = "thran"
number = 1.2
not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))
letter = (\ x -> (let { y = x } in y))
integer = 7
identity = (\ x -> x)
character = 't'
case_ = (\ x -> (case (x) of { (y) -> y }))
boolean = Prelude.True
array = [1, 2, 3]
apply = (\ f -> (\ x -> (f x)))
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

Currently Thran does not support:

- Record literals
- Module imports
- Foreign imports
- Recursive declarations
- Data constructors
- Newtype wrappers
- Type classes
- Guard clauses

Thran is a proof of concept at this point.
Don't use it for anything serious.
