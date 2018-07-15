{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import SDL
import SDL.Vect (Point(..))
import Linear (V4(..))
import Control.Monad (unless)
import Foreign.C.Types (CInt(..))
import qualified Debug.Trace as Debug (trace)
import Data.Word (Word8(..))
import qualified Data.Vector.Storable as Vector.Storable
import Protolude

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer $= (Just (V2 2 2))
--  rendererClipRect renderer $= (Just (Rectangle (P (V2 0 0)) (V2 2 2)))
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  event <- waitEvent
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = eventIsQPress event

  case eventPayload event of
    MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ coords) -> print coords
    _ -> return ()

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P (V2 0 0)) (V2 1 1)))
  rendererDrawColor renderer $= V4 0 255 0 255
  fillRect renderer (Just (Rectangle (P (V2 1 0)) (V2 1 1)))
  rendererDrawColor renderer $= V4 0 0 255 255
  fillRect renderer (Just (Rectangle (P (V2 0 1)) (V2 1 1)))
  rendererDrawColor renderer $= V4 100 150 200 255
  fillRect renderer (Just (Rectangle (P (V2 1 1)) (V2 1 1)))

  present renderer
  if not qPressed then (appLoop renderer) else SDL.quit
