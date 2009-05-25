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
getTicks = liftM toInteger inHSDLGetTicks

delay :: Integer -> IO ()
delay = inHSDLDelay . fromInteger

addTimer :: Integer -> (Integer -> IO Integer) -> IO TimerID
addTimer interval cb = do
  fp <- mkNewCallback cb
  inHSDLAddTimer (fromInteger interval) fp nullPtr

mkNewCallback f = mkNTC inner where
  inner itv adv = do
    ret <- f $ toInteger itv
    return $ fromInteger ret

foreign import ccall "wrapper" mkNTC :: NewTimerCallback -> IO (FunPtr NewTimerCallback)

removeTimer :: TimerID -> IO Bool
removeTimer id = liftM toBool $ inHSDLRemoveTimer id

setTimer :: Integer -> (Integer -> IO Integer) -> IO ()
setTimer interval cb = do
  fp  <- mkCallback cb
  ret <- inHSDLSetTimer (fromInteger interval) fp -- •Ô‚è’l‚ÌˆÓ–¡‚ª‚í‚©‚ç‚ñ
  return ()

mkCallback f = mkTC inner where
  inner itv = do
    ret <- f $ toInteger itv
    return $ fromInteger ret

foreign import ccall "wrapper" mkTC  :: TimerCallback    -> IO (FunPtr TimerCallback)

----------

#include <HSDL.h>
#undef main

foreign import ccall "HSDL.h HSDL_GetTicks"    inHSDLGetTicks    :: IO Word32
foreign import ccall "HSDL.h HSDL_Delay"       inHSDLDelay       :: Word32 -> IO ()
foreign import ccall "HSDL.h HSDL_AddTimer"    inHSDLAddTimer    :: Word32 -> FunPtr NewTimerCallback -> Ptr () -> IO TimerID
foreign import ccall "HSDL.h HSDL_RemoveTimer" inHSDLRemoveTimer :: TimerID -> IO Int
foreign import ccall "HSDL.h HSDL_SetTimer"    inHSDLSetTimer    :: Word32 -> FunPtr TimerCallback -> IO Int
