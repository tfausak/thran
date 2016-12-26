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
  , err :: EXCEPTION
  , fs :: FS
  , process :: PROCESS
  ) Unit
main = do
  arguments <- Process.argv
  case arguments of
    [_, _, file] -> FS.readTextFile Encoding.UTF8 file \ result ->
      case result of
        Either.Right contents -> case Argonaut.jsonParser contents of
          Either.Right json -> case Thran.compile json of
            Either.Right haskell -> Console.log haskell
            Either.Left message -> Exception.throw message
          Either.Left message -> Exception.throw message
        Either.Left error -> Exception.throwException error
    _ -> Exception.throw "invalid arguments"
