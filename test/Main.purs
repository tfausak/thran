module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff as Eff
import Data.Argonaut as Argonaut
import Data.Either as Either
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
foreign import nullCaseCoreFn :: Argonaut.Json
foreign import numberCoreFn :: Argonaut.Json
foreign import objectCoreFn :: Argonaut.Json
foreign import newtypeCoreFn :: Argonaut.Json
foreign import nonEmptyObjectCoreFn :: Argonaut.Json
foreign import recordAccessCoreFn :: Argonaut.Json
foreign import stringCoreFn :: Argonaut.Json
foreign import typeClassCoreFn :: Argonaut.Json

main :: Eff.Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  ) Unit
main = Main.runTest do
  Test.suite "Thran" do
    Test.suite "compile" do
      Test.test "nothing" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
()
where
import qualified Bookkeeper
import qualified Prelude
"""
        let actual = Thran.compile emptyCoreFn
        Assert.equal expected actual
      Test.test "function declaration" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(identity)
where
import qualified Bookkeeper
import qualified Prelude
identity = (\ x -> x)
"""
        let actual = Thran.compile functionCoreFn
        Assert.equal expected actual
      Test.test "function application" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(apply)
where
import qualified Bookkeeper
import qualified Prelude
apply = (\ f -> (\ x -> (f x)))
"""
        let actual = Thran.compile applicationCoreFn
        Assert.equal expected actual
      Test.test "boolean literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(boolean)
where
import qualified Bookkeeper
import qualified Prelude
boolean = Prelude.False
"""
        let actual = Thran.compile booleanCoreFn
        Assert.equal expected actual
      Test.test "integer literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(int)
where
import qualified Bookkeeper
import qualified Prelude
int = 0
"""
        let actual = Thran.compile integerCoreFn
        Assert.equal expected actual
      Test.test "number literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(number)
where
import qualified Bookkeeper
import qualified Prelude
number = 0.0
"""
        let actual = Thran.compile numberCoreFn
        Assert.equal expected actual
      Test.test "character literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(char)
where
import qualified Bookkeeper
import qualified Prelude
char = 'a'
"""
        let actual = Thran.compile characterCoreFn
        Assert.equal expected actual
      Test.test "string literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(string)
where
import qualified Bookkeeper
import qualified Prelude
string = ""
"""
        let actual = Thran.compile stringCoreFn
        Assert.equal expected actual
      Test.test "array literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(array)
where
import qualified Bookkeeper
import qualified Prelude
array = [0, 1]
"""
        let actual = Thran.compile arrayCoreFn
        Assert.equal expected actual
      Test.test "case expression" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(identity)
where
import qualified Bookkeeper
import qualified Prelude
identity = (\ x -> (case (x) of { (y) -> y }))
"""
        let actual = Thran.compile caseCoreFn
        Assert.equal expected actual
      Test.test "conditional expression" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(not)
where
import qualified Bookkeeper
import qualified Prelude
not = (\ x -> (case (x) of { (Prelude.True) -> Prelude.False; (Prelude.False) -> Prelude.True }))
"""
        let actual = Thran.compile conditionalCoreFn
        Assert.equal expected actual
      Test.test "case expression with multiple binders" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(f)
where
import qualified Bookkeeper
import qualified Prelude
f = (\ x -> (case (x, x) of { (y, z) -> x }))
"""
        let actual = Thran.compile multipleCaseCoreFn
        Assert.equal expected actual
      Test.test "case expression with null binder" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(f)
where
import qualified Bookkeeper
import qualified Prelude
f = (\ x -> (case (x) of { (_) -> x }))
"""
        let actual = Thran.compile nullCaseCoreFn
        Assert.equal expected actual
      Test.test "let expression" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(f)
where
import qualified Bookkeeper
import qualified Prelude
f = (\ x -> (let { y = x } in y))
"""
        let actual = Thran.compile letCoreFn
        Assert.equal expected actual
      Test.test "intra-module identifier reference" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(f, g)
where
import qualified Bookkeeper
import qualified Prelude
f = (\ x -> x)
g = M.f
"""
        let actual = Thran.compile identifierCoreFn
        Assert.equal expected actual
      Test.test "interesting module name" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module Aa1.Bb1
()
where
import qualified Bookkeeper
import qualified Prelude
"""
        let actual = Thran.compile moduleNameCoreFn
        Assert.equal expected actual
      Test.test "object literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(x)
where
import qualified Bookkeeper
import qualified Prelude
x = (Bookkeeper.emptyBook)
"""
        let actual = Thran.compile objectCoreFn
        Assert.equal expected actual
      Test.test "non-empty object literal" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(x)
where
import qualified Bookkeeper
import qualified Prelude
x = (Bookkeeper.emptyBook Bookkeeper.& #a Bookkeeper.=: 1)
"""
        let actual = Thran.compile nonEmptyObjectCoreFn
        Assert.equal expected actual
      Test.test "record access" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(f)
where
import qualified Bookkeeper
import qualified Prelude
f = (\ x -> (Bookkeeper.get #k x))
"""
        let actual = Thran.compile recordAccessCoreFn
        Assert.equal expected actual
      Test.test "newtype" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(_X)
where
import qualified Bookkeeper
import qualified Prelude
_X = (\ x -> x)
"""
        let actual = Thran.compile newtypeCoreFn
        Assert.equal expected actual
      Test.test "type class" do
        let expected = Either.Right """{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
-- Built with psc version 0.10.3.
module M
(_Semigroup, append)
where
import qualified Bookkeeper
import qualified Prelude
_Semigroup = (\ append -> (Bookkeeper.emptyBook Bookkeeper.& #append Bookkeeper.=: append))
append = (\ dict -> (Bookkeeper.get #append dict))
"""
        let actual = Thran.compile typeClassCoreFn
        Assert.equal expected actual
