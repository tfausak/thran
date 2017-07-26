module Main where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.Process (PROCESS)

import Control.Monad.Eff as Eff
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception as Exception
import Data.Argonaut as Argonaut
import Data.Either as Either
import Node.Encoding as Encoding
import Node.FS.Async as FS
import Node.Process as Process
import Thran as Thran

main :: Eff.Eff
  ( console :: CONSOLE
  , exception :: EXCEPTION
  , fs :: FS
  , process :: PROCESS
  ) Unit
main = do
  arguments <- Process.argv
  file <- case arguments of
    [_, _, x] -> pure x
    _ -> Exception.throw "invalid arguments"
  FS.readTextFile Encoding.UTF8 file \ result -> do
    contents <- case result of
      Either.Right x -> pure x
      Either.Left x -> Exception.throwException x
    json <- case Argonaut.jsonParser contents of
      Either.Right x -> pure x
      Either.Left x -> Exception.throw x
    haskell <- case Thran.compile json of
      Either.Right x -> pure x
      Either.Left x -> Exception.throw x
    Console.log haskell
