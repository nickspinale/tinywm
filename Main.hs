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

import Graphics.XHB
import Graphics.XHB.Connection
import Graphics.XHB.Monad
import Graphics.XHB.EventQueue
import Graphics.XHB.MappingState
import Graphics.XHB.KeySym.Defs


main :: IO ()
main = do
    mconn <- connect
    case mconn of
        Nothing -> die "failed to connect to x server"
        Just conn -> do
            end <- unX (runMappingT (runEventQueueT tinywm)) conn
            case end of
                Left err -> die (show err)
                Right _ -> return ()


noneId :: XidLike id => id
noneId = fromXid xidNone

currentTime :: TIMESTAMP
currentTime = noneId

tinywm :: (MonadX IO m, EventQueueCtx m, MappingCtx m) => m ()
tinywm = void $ do
    root <- asksX getRoot
    km <- getsMapping keyMap
    forM (keyCodesOf xK_x km) $ \kc -> notify $ MkGrabKey True root [ModMask1] kc GrabModeAsync GrabModeAsync
    let grabButton ix = notify $ MkGrabButton True root [EventMaskButtonPress] GrabModeAsync GrabModeAsync noneId noneId ix [ModMask1]
    grabButton ButtonIndex1
    grabButton ButtonIndex3
    evalStateT (forever eventLoop) Nothing


type TinyState = Maybe (WINDOW, Int16, Int16, MotionState)

data MotionState = Moving Int16 Int16 | Resizing Word16 Word16

data EventHandler a = forall e. Event e => EventHandler (e -> a)

handle :: MonadX x m => [EventHandler (m ())] -> SomeEvent -> m ()
handle hs ev = fromMaybe (return ()) $ asum [ h `fmap` fromEvent ev | EventHandler h <- hs ]

eventLoop :: (MonadX IO m, EventQueueCtx m) => StateT TinyState m ()
eventLoop = lift waitEvent >>= handle
    [ EventHandler onKeyPress
    , EventHandler onButtonPress
    , EventHandler onButtonRelease
    , EventHandler onMotionNotify
    ]


onKeyPress :: (MonadX IO m, MonadState TinyState m) => KeyPressEvent -> m ()
onKeyPress MkKeyPressEvent{..} = notify $ MkConfigureWindow child_KeyPressEvent param
  where
    param = toValueParam [(ConfigWindowStackMode, toValue StackModeAbove)]


onButtonPress :: (MonadX IO m, MonadState TinyState m) => ButtonPressEvent -> m ()
onButtonPress MkButtonPressEvent{..} = do
    when (child /= noneId) $ do
        MkGetGeometryReply{..} <- req . MkGetGeometry . fromXid $ toXid child
        let continue motion = do
            let mask = [EventMaskPointerMotion, EventMaskButtonRelease]
            MkGrabPointerReply status <- req $ MkGrabPointer True child mask GrabModeAsync GrabModeAsync noneId noneId currentTime
            case status of
                GrabStatusSuccess -> put $ Just (child, root_x_ButtonPressEvent, root_y_ButtonPressEvent, motion)
                _ -> return ()
        case detail_ButtonPressEvent of
            1 -> continue $ Moving x_GetGeometryReply y_GetGeometryReply
            3 -> continue $ Resizing width_GetGeometryReply height_GetGeometryReply
            _ -> return ()
  where
    child = child_ButtonPressEvent


onButtonRelease :: (MonadX IO m, MonadState TinyState m) => ButtonReleaseEvent -> m ()
onButtonRelease MkButtonReleaseEvent{..} = do
    put Nothing
    notify $ MkUngrabPointer currentTime


onMotionNotify :: (MonadX IO m, EventQueueCtx m, MonadState TinyState m) => MotionNotifyEvent -> m ()
onMotionNotify ev = do
    flushEventQueue
    MkMotionNotifyEvent{..} <- fromMaybe ev <$> skipEventsByType
    st <- get
    case st of
        Nothing -> return ()
        Just (win, xroot, yroot, motion) -> do
            let xdiff = root_x_MotionNotifyEvent - xroot
                ydiff = root_y_MotionNotifyEvent - yroot
                vals = case motion of
                    Moving x0 y0 -> [(ConfigWindowX, fromIntegral $ x0 + xdiff), (ConfigWindowY, fromIntegral $ y0 + ydiff)]
                    Resizing width0 height0 ->
                        -- TODO weird behavior at small widths
                        let bound n = fromIntegral $ if n > 64 then n else 64
                            width = bound $ (fromIntegral width0 :: Int16) + xdiff
                            height = bound $ (fromIntegral height0 :: Int16) + ydiff
                        in [(ConfigWindowWidth, width), (ConfigWindowHeight, height)]
            notify $ MkConfigureWindow win $ toValueParam vals
