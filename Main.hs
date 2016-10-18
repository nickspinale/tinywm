{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Maybe
import Data.Foldable

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState
import Graphics.XHB.KeySym.Defs

import Control.Monad
import System.Exit


main :: IO ()
main = connect >>= maybe (die "failed to connect to x server") (setCrashOnError >> (unX (runMappingT tinywm) >=> either (die . show) return))

tinywm :: (MonadX IO m, MappingCtx m) => m a
tinywm = forever $ awaitEv >>= handles_ [EventHandler $ \ev@MkKeyPressEvent{..} -> liftX (print ev)]


-- HANDLERS

data EventHandler a = forall e. Event e => EventHandler (e -> a)

handle :: EventHandler a -> SomeEvent -> Maybe a
handle (EventHandler h) ev = h `fmap` fromEvent ev

handles :: [EventHandler a] -> SomeEvent -> Maybe a
handles hs ev = asum $ map (flip handle ev) hs

handles_ :: MonadX x m => [EventHandler (m ())] -> SomeEvent -> m ()
handles_ = (.) (fromMaybe $ return ()) . handles


-- CONSTANTS

noneId :: XidLike id => id
noneId = fromXid xidNone

noSymbol :: KEYSYM
noSymbol = 0
