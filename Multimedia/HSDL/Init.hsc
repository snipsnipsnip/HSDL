{-# OPTIONS -fglasgow-exts #-}
module Multimedia.HSDL.Init(
  Subsystem(..),

  sdlInit, initSubSystem,
  sdlQuit, quitSubSystem,

  wasInit,
  getError,

) where

import Foreign
import Foreign.C
import Control.Monad
import Multimedia.HSDL.Util

data Subsystem =
    TIMER | AUDIO | VIDEO | CDROM | JOYSTICK
  | EVERYTHING | NOPARACHUTE | EVENTTHREAD
  deriving (Eq,Show,Enum)

instance Flag Subsystem where
  fromFlag TIMER       = #const SDL_INIT_TIMER
  fromFlag AUDIO       = #const SDL_INIT_AUDIO
  fromFlag VIDEO       = #const SDL_INIT_VIDEO
  fromFlag CDROM       = #const SDL_INIT_CDROM
  fromFlag JOYSTICK    = #const SDL_INIT_JOYSTICK
  fromFlag NOPARACHUTE = #const SDL_INIT_NOPARACHUTE
  fromFlag EVENTTHREAD = #const SDL_INIT_EVENTTHREAD
  fromFlag EVERYTHING  = #const SDL_INIT_EVERYTHING

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
