# Thran

Thran compiles PureScript into Haskell.
It is written in PureScript.

To use Thran, first compile a PureScript module with `psc --dump-corefn`.
Then convert the corefn into Haskell.

``` shell
$ psc --dump-corefn path-to/Example.purs
$ npm run thran path-to/Example/corefn.json > path-to/Example.hs
```

For example, given the following PureScript module:

``` purescript
module Example where
string = "thran"
number = 1.2
integer = 7
identity x = x
character = 't'
boolean = true
array = [1, 2, 3]
apply f x = f x
```

Thran generates this Haskell module:

``` haskell
-- Built with psc version 0.10.3.
module Example
(apply, array, boolean, character, identity, integer, number, string)
where
string = "thran"
number = 1.2
integer = 7
identity = (\ x -> x)
character = 't'
boolean = True
array = [1, 2, 3]
apply = (\ f -> (\ x -> (f x)))
```

Thran is still a young project.
So far, Thran supports:

- Module export lists
- Top-level non-recursive declarations
- Array, boolean, character, function, integer, number, and string literals

Currently Thran does not support:

- Record literals
- Module imports
- Foreign imports
- Case expressions, including `if _ then _ else _`
- Recursive declarations
- Data constructors
- Newtype wrappers

Thran is a proof of concept at this point.
Don't use it for anything serious.
