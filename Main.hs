module Main (main) where

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState
import Graphics.XHB.KeySym.Defs

import Control.Monad
import System.Exit

main :: IO ()
main = connect >>= maybe (die "failed to connect to x server") (unX (runMappingT tinywm) >=> either (die . show) return)

tinywm :: (MonadX x m, MappingCtx m) => m a
tinywm = undefined
