module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Reader.Class (ask)
import Control.Monad.ST (runST, newSTRef, writeSTRef, readSTRef)

import Config (Latest, Timer, ConfigReader, runConfigReader)

import RxJS.Observable (Observable, subscribeNext, timer)

main :: Eff (console :: CONSOLE) Unit
main = runST do
  st <- newSTRef Nothing
  runConfigReader (ask :: ConfigReader Observable (Latest Timer)) $ \cfg -> do
    void $ writeSTRef st (Just cfg)

  void $ subscribeNext (\_ -> do
    x <- readSTRef st
    log $ show x) (timer 0 1333)
