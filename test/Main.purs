module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff as Eff
import Data.Argonaut as Argonaut
import Data.Either as Either
import Data.String as String
import Test.Unit as Test
import Test.Unit.Assert as Assert
import Test.Unit.Main as Main
import Thran as Thran

foreign import applicationCoreFn :: Argonaut.Json
foreign import arrayCoreFn :: Argonaut.Json
foreign import booleanCoreFn :: Argonaut.Json
foreign import caseCoreFn :: Argonaut.Json
foreign import characterCoreFn :: Argonaut.Json
foreign import conditionalCoreFn :: Argonaut.Json
foreign import emptyCoreFn :: Argonaut.Json
foreign import functionCoreFn :: Argonaut.Json
foreign import identifierCoreFn :: Argonaut.Json
foreign import integerCoreFn :: Argonaut.Json
foreign import letCoreFn :: Argonaut.Json
foreign import moduleNameCoreFn :: Argonaut.Json
foreign import multipleCaseCoreFn :: Argonaut.Json
foreign import newtypeCoreFn :: Argonaut.Json
foreign import nonEmptyObjectCoreFn :: Argonaut.Json
foreign import nullCaseCoreFn :: Argonaut.Json
foreign import numberCoreFn :: Argonaut.Json
foreign import objectCoreFn :: Argonaut.Json
foreign import partialCoreFn :: Argonaut.Json
foreign import recordAccessCoreFn :: Argonaut.Json
foreign import semigroupCoreFn :: Argonaut.Json
foreign import stringCoreFn :: Argonaut.Json
foreign import superClassCoreFn :: Argonaut.Json
foreign import typeClassCoreFn :: Argonaut.Json

main :: Eff.Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  ) Unit
main = Main.runTest do
  Test.suite "Thran" do
    Test.suite "compile" do

      test "nothing" emptyCoreFn "M" []
        [
        ]

      test "function" functionCoreFn "M" ["identity"]
        [ "identity = (\\ x -> x)"
        ]

      test "application" applicationCoreFn "M" ["apply"]
        [ "apply = (\\ f -> (\\ x -> (f x)))"
        ]

      test "boolean" booleanCoreFn "M" ["boolean"]
        [ "boolean = Prelude.False"
        ]

      test "integer" integerCoreFn "M" ["int"]
        [ "int = 0"
        ]

      test "number" numberCoreFn "M" ["number"]
        [ "number = 0.0"
        ]

      test "character" characterCoreFn "M" ["char"]
        [ "char = 'a'"
        ]

      test "string" stringCoreFn "M" ["string"]
        [ "string = \"\""
        ]

      test "array" arrayCoreFn "M" ["array"]
        [ "array = [0, 1]"
        ]

      test "case" caseCoreFn "M" ["identity"]
        [ "identity = (\\ x -> (case (x) of { (y) -> y }))"
        ]

      test "conditional" conditionalCoreFn "M" ["not"]
        [ "not = (\\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))"
        ]

      test "multiple case" multipleCaseCoreFn "M" ["f"]
        [ "f = (\\ x -> (case (x, x) of { (y, z) -> x }))"
        ]

      test "null case" nullCaseCoreFn "M" ["f"]
        [ "f = (\\ x -> (case (x) of { (_) -> x }))"
        ]

      test "let" letCoreFn "M" ["f"]
        [ "f = (\\ x -> (let { y = x } in y))"
        ]

      test "intra-module reference" identifierCoreFn "M" ["f", "g"]
        [ "f = (\\ x -> x)"
        , "g = M.f"
        ]

      test "interesting module name" moduleNameCoreFn "Aa1.Bb1" []
        [
        ]

      test "empty record" objectCoreFn "M" ["x"]
        [ "x = (Bookkeeper.emptyBook)"
        ]

      test "non-empty record" nonEmptyObjectCoreFn "M" ["x"]
        [ "x = (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"a\")) Bookkeeper.=: 1)"
        ]

      test "field access" recordAccessCoreFn "M" ["f"]
        [ "f = (\\ x -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"k\")) x))"
        ]

      test "newtype" newtypeCoreFn "M" ["_X"]
        [ "_X = (\\ x -> x)"
        ]

      test "type class" typeClassCoreFn "M" ["_Semigroup", "append"]
        [ "_Semigroup = (\\ append -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"append\")) Bookkeeper.=: append))"
        , "append = (\\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"append\")) dict))"
        ]

      test "super class" superClassCoreFn "M" ["_A", "_B"]
        [ "_A = (Bookkeeper.emptyBook)"
        , "_B = (\\ __superclass_M__A_0 -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"__superclass_M.A_0\")) Bookkeeper.=: __superclass_M__A_0))"
        ]

      test "using type class" semigroupCoreFn "Example" ["_Semigroup", "append", "triple"]
        [ "_Semigroup = (\\ append -> (Bookkeeper.emptyBook Bookkeeper.& (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"append\")) Bookkeeper.=: append))"
        , "append = (\\ dict -> (Bookkeeper.get (GHC.OverloadedLabels.fromLabel (GHC.Prim.proxy# :: GHC.Prim.Proxy# \"append\")) dict))"
        , "triple = (\\ dictSemigroup -> (\\ x -> (((Example.append dictSemigroup) (((Example.append dictSemigroup) x) x)) x)))"
        ]

      test "partial" partialCoreFn "Example" ["partial"]
        [ "partial = (\\ dictPartial -> (\\ v -> (let { __unused = (\\ dictPartial1 -> (\\ _Dollar_2 -> _Dollar_2)) } in ((__unused dictPartial) (case (v) of { (0) -> 0 })))))"
        ]

test :: forall e. String -> Argonaut.Json -> String -> Array String -> Array String -> Test.TestSuite e
test name corefn moduleName exports declarations =
  Test.test name do
    let expected = formatModule moduleName exports declarations
    let actual = Thran.compile corefn
    Assert.equal (Either.Right expected) actual

formatModule :: String -> Array String -> Array String -> String
formatModule name exports declarations = do
  let formattedExports = formatExports exports
  let formattedDeclarations = formatDeclarations declarations
  let rawModule = String.joinWith ""
        [ """-- Built with psc version 0.10.3.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module """
        , name
        , """ (
"""
        , formattedExports
        , """) where

import qualified Bookkeeper
import qualified GHC.OverloadedLabels
import qualified GHC.Prim
import qualified Prelude
"""
        , formattedDeclarations
        ]
  normalizeNewlines rawModule

formatExports :: Array String -> String
formatExports =
  map (\ export -> "  " <> export <> ",\n")
  >>> String.joinWith ""

formatDeclarations :: Array String -> String
formatDeclarations =
  String.joinWith "\n\n"
  >>> (_ <> "\n")
  >>> ("\n" <> _)

normalizeNewlines :: String -> String
normalizeNewlines =
  String.replaceAll (String.Pattern "\r\n") (String.Replacement "\n")
