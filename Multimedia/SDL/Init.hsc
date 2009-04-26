{-# OPTIONS -fglasgow-exts #-}
module Multimedia.SDL.Init(
  Subsystem(..),

  sdlInit, initSubSystem,
  sdlQuit, quitSubSystem,

  wasInit,
  getError,

) where

import Foreign
import Foreign.C
import Control.Monad
import Multimedia.SDL.Util

data Subsystem =
    TIMER | AUDIO | VIDEO | CDROM | JOYSTICK
  | EVERYTHING | NOPARACHUTE | EVENTTHREAD
  deriving (Eq,Show,Enum)

instance Flag Subsystem where
  fromFlag TIMER       = 0x00000001
  fromFlag AUDIO       = 0x00000010
  fromFlag VIDEO       = 0x00000020
  fromFlag CDROM       = 0x00000100
  fromFlag JOYSTICK    = 0x00000200
  fromFlag NOPARACHUTE = 0x00100000
  fromFlag EVENTTHREAD = 0x01000000
  fromFlag EVERYTHING  = 0x0000FFFF

----------

sdlInit :: [Subsystem] -> IO Bool
sdlInit ss = do
  ret <- inSDLInit $ fromFlags ss
  return $ ret>=0

initSubSystem :: [Subsystem] -> IO Bool
initSubSystem ss = do
  ret <- inSDLInitSubSystem $ fromFlags ss
  return $ ret>=0

sdlQuit :: IO ()
sdlQuit = inSDLQuit

quitSubSystem :: [Subsystem] -> IO ()
quitSubSystem = inSDLQuitSubSystem . fromFlags

wasInit :: [Subsystem] -> IO [Subsystem]
wasInit ss = liftM toFlags $ inSDLWasInit $ fromFlags ss

getError :: IO String
getError = do
  p <- inSDLGetError
  peekCString p

----------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_Init"          inSDLInit :: Word32 -> IO Int
foreign import ccall "SDL.h SDL_InitSubSystem" inSDLInitSubSystem :: Word32 -> IO Int
foreign import ccall "SDL.h SDL_Quit"          inSDLQuit :: IO ()
foreign import ccall "SDL.h SDL_QuitSubSystem" inSDLQuitSubSystem :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_WasInit"       inSDLWasInit :: Word32 -> IO Word32
foreign import ccall "SDL.h SDL_GetError"      inSDLGetError :: IO CString
