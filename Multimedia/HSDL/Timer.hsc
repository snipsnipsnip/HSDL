{-# OPTIONS -fglasgow-exts #-}
module Multimedia.HSDL.Timer(
  TimerID,

  getTicks,
  delay,

  addTimer,
  removeTimer,
  setTimer,

) where

import Foreign
import Control.Monad

type TimerID = Ptr ()

type NewTimerCallback = Word32 -> Ptr () -> IO Word32
type TimerCallback    = Word32 -> IO Word32

----------

getTicks :: IO Integer
getTicks = liftM toInteger inSDLGetTicks

delay :: Integer -> IO ()
delay = inSDLDelay . fromInteger

addTimer :: Integer -> (Integer -> IO Integer) -> IO TimerID
addTimer interval cb = do
  fp <- mkNewCallback cb
  inSDLAddTimer (fromInteger interval) fp nullPtr

mkNewCallback f = mkNTC inner where
  inner itv adv = do
    ret <- f $ toInteger itv
    return $ fromInteger ret

foreign import ccall "wrapper" mkNTC :: NewTimerCallback -> IO (FunPtr NewTimerCallback)

removeTimer :: TimerID -> IO Bool
removeTimer id = liftM toBool $ inSDLRemoveTimer id

setTimer :: Integer -> (Integer -> IO Integer) -> IO ()
setTimer interval cb = do
  fp  <- mkCallback cb
  ret <- inSDLSetTimer (fromInteger interval) fp -- •Ô‚è’l‚ÌˆÓ–¡‚ª‚í‚©‚ç‚ñ
  return ()

mkCallback f = mkTC inner where
  inner itv = do
    ret <- f $ toInteger itv
    return $ fromInteger ret

foreign import ccall "wrapper" mkTC  :: TimerCallback    -> IO (FunPtr TimerCallback)

----------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_GetTicks"    inSDLGetTicks    :: IO Word32
foreign import ccall "SDL.h SDL_Delay"       inSDLDelay       :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_AddTimer"    inSDLAddTimer    :: Word32 -> FunPtr NewTimerCallback -> Ptr () -> IO TimerID
foreign import ccall "SDL.h SDL_RemoveTimer" inSDLRemoveTimer :: TimerID -> IO Int
foreign import ccall "SDL.h SDL_SetTimer"    inSDLSetTimer    :: Word32 -> FunPtr TimerCallback -> IO Int
