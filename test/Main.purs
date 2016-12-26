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

foreign import emptyCoreFn :: Argonaut.Json
foreign import functionCoreFn :: Argonaut.Json
foreign import applicationCoreFn :: Argonaut.Json
foreign import booleanCoreFn :: Argonaut.Json
foreign import intCoreFn :: Argonaut.Json
foreign import numberCoreFn :: Argonaut.Json
foreign import charCoreFn :: Argonaut.Json
foreign import stringCoreFn :: Argonaut.Json
foreign import arrayCoreFn :: Argonaut.Json

main :: Eff.Eff
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  ) Unit
main = Main.runTest do
  Test.suite "Thran" do
    Test.suite "compile" do
      Test.test "nothing" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
()
where
"""
        let actual = Thran.compile emptyCoreFn
        Assert.equal expected actual
      Test.test "function" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(identity)
where
identity = (\ x -> x)
"""
        let actual = Thran.compile functionCoreFn
        Assert.equal expected actual
      Test.test "function application" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(apply)
where
apply = (\ f -> (\ x -> (f x)))
"""
        let actual = Thran.compile applicationCoreFn
        Assert.equal expected actual
      Test.test "boolean literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(boolean)
where
boolean = False
"""
        let actual = Thran.compile booleanCoreFn
        Assert.equal expected actual
      Test.test "integer literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(int)
where
int = 0
"""
        let actual = Thran.compile intCoreFn
        Assert.equal expected actual
      Test.test "number literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(number)
where
number = 0.0
"""
        let actual = Thran.compile numberCoreFn
        Assert.equal expected actual
      Test.test "character literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(char)
where
char = 'a'
"""
        let actual = Thran.compile charCoreFn
        Assert.equal expected actual
      Test.test "string literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(string)
where
string = ""
"""
        let actual = Thran.compile stringCoreFn
        Assert.equal expected actual
      Test.test "array literal" do
        let expected = Either.Right """-- Built with psc version 0.10.3.
module M
(array)
where
array = [0, 1]
"""
        let actual = Thran.compile arrayCoreFn
        Assert.equal expected actual
