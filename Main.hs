{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Int
import Data.Word
import Data.Maybe
import Data.Foldable
import System.Exit
import System.IO
import System.Process

import Graphics.XHB
import Graphics.XHB.Connection
import Graphics.XHB.Monad
import Graphics.XHB.MappingState
import Graphics.XHB.MappingState.Internal
import Graphics.XHB.KeySym.Defs


main :: IO ()
main = connect >>= maybe (die "failed to connect to x server") (setCrashOnError >> (unX (runMappingT tinywm) >=> either (die . show) return))


tinywm :: (MonadX IO m, MappingCtx m) => m ()
tinywm = void $ do
    liftX $ hSetBuffering stdout LineBuffering
    root <- asksX getRoot
    km <- getsMapping keyMap
    forM_ (keyCodesOf xK_x km) $ \kc -> notify $ MkGrabKey True root [ModMask1] kc GrabModeAsync GrabModeAsync
    let grabButton ix = notify $ MkGrabButton True root [EventMaskButtonPress] GrabModeAsync GrabModeAsync noneId noneId ix [ModMask1]
    grabButton ButtonIndex1
    grabButton ButtonIndex3
    evalStateT eventLoop Nothing


-- HANDLING


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

currentTime :: TIMESTAMP
currentTime = noneId

noSymbol :: KEYSYM
noSymbol = 0


-- EVENT LOOP


type TinyState = Maybe (WINDOW, Int16, Int16, MotionState)

data MotionState = Moving Int16 Int16 | Resizing Word16 Word16


eventLoop :: MonadX IO m => StateT TinyState m ()
eventLoop = forever $ lift awaitEv >>= handles_
    [ EventHandler onKeyPress
    , EventHandler onButtonPress
    , EventHandler onButtonRelease
    , EventHandler onMotionNotify
    ]


onKeyPress :: (MonadX IO m, MonadState TinyState m) => KeyPressEvent -> m ()
onKeyPress MkKeyPressEvent{..} = notify . MkConfigureWindow child_KeyPressEvent $ toValueParam [(ConfigWindowStackMode, toValue StackModeAbove)]


onButtonPress :: (MonadX IO m, MonadState TinyState m) => ButtonPressEvent -> m ()
onButtonPress MkButtonPressEvent{..} = do
    when (child_ButtonPressEvent /= noneId) $ do
        MkGetGeometryReply{..} <- req . MkGetGeometry . fromXid . toXid $ child_ButtonPressEvent
        let continue motion = do
            MkGrabPointerReply status <- req $ MkGrabPointer True child_ButtonPressEvent [EventMaskPointerMotion, EventMaskButtonRelease] GrabModeAsync GrabModeAsync noneId noneId currentTime
            case status of
                GrabStatusSuccess -> put $ Just (child_ButtonPressEvent, root_x_ButtonPressEvent, root_y_ButtonPressEvent, motion)
                _ -> return ()
        case detail_ButtonPressEvent of
            1 -> continue $ Moving x_GetGeometryReply y_GetGeometryReply
            3 -> continue $ Resizing width_GetGeometryReply height_GetGeometryReply
            _ -> return ()


onButtonRelease :: (MonadX IO m, MonadState TinyState m) => ButtonReleaseEvent -> m ()
onButtonRelease MkButtonReleaseEvent{..} = do
    put Nothing
    notify $ MkUngrabPointer currentTime



onMotionNotify :: (MonadX IO m, MonadState TinyState m) => MotionNotifyEvent -> m ()
onMotionNotify MkMotionNotifyEvent{..} = do
    st <- get
    case st of
        Nothing -> return ()
        Just (win, xroot, yroot, motion) -> do
            let xdiff = root_x_MotionNotifyEvent - xroot
                ydiff = root_y_MotionNotifyEvent - yroot
                vals = case motion of
                    Moving x0 y0 -> [(ConfigWindowX, fromIntegral $ x0 + xdiff), (ConfigWindowY, fromIntegral $ y0 + ydiff)]
                    Resizing width0 height0 ->
                        let or1 n = fromIntegral $ if n > 1 then n else 1
                            width = or1 $ fromIntegral width0 + xdiff
                            height = or1 $ fromIntegral height0 + ydiff
                        in [(ConfigWindowWidth, width), (ConfigWindowHeight, height)]
            notify $ MkConfigureWindow win $ toValueParam vals
