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
  ret <- inHSDLInit $ fromFlags ss
  return $ ret>=0

initSubSystem :: [Subsystem] -> IO Bool
initSubSystem ss = do
  ret <- inHSDLInitSubSystem $ fromFlags ss
  return $ ret>=0

sdlQuit :: IO ()
sdlQuit = inHSDLQuit

quitSubSystem :: [Subsystem] -> IO ()
quitSubSystem = inHSDLQuitSubSystem . fromFlags

wasInit :: [Subsystem] -> IO [Subsystem]
wasInit ss = liftM toFlags $ inHSDLWasInit $ fromFlags ss

getError :: IO String
getError = do
  p <- inHSDLGetError
  peekCString p

----------

#include <HSDL.h>
#undef main

foreign import ccall "HSDL.h HSDL_Init"          inHSDLInit :: Word32 -> IO Int
foreign import ccall "HSDL.h HSDL_InitSubSystem" inHSDLInitSubSystem :: Word32 -> IO Int
foreign import ccall "HSDL.h HSDL_Quit"          inHSDLQuit :: IO ()
foreign import ccall "HSDL.h HSDL_QuitSubSystem" inHSDLQuitSubSystem :: Word32 -> IO ()
foreign import ccall "HSDL.h HSDL_WasInit"       inHSDLWasInit :: Word32 -> IO Word32
foreign import ccall "HSDL.h HSDL_GetError"      inHSDLGetError :: IO CString