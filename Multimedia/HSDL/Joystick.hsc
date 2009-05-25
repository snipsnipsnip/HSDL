{-# OPTIONS -fglasgow-exts #-}
module Multimedia.HSDL.Joystick(
  Joystick, HatState(..),

  numJoysticks,
  joystickName,
  joystickOpen,
  joystickOpened,
  joystickIndex,
  joystickNumAxes,
  joystickNumBalls,
  joystickNumHats,
  joystickNumButtons,
  joystickUpdate,
  joystickGetAxis,
  joystickGetHat,
  joystickGetButton,
  joystickGetBall,
  joystickClose,

) where

import Foreign
import Foreign.C
import Control.Monad
import Multimedia.HSDL.Util

type Joystick = Ptr ()

data HatState =
    HAT_CENTERD
  | HAT_UP | HAT_RIGHT | HAT_DOWN | HAT_LEFT
  | HAT_RIGHTUP | HAT_RIGHTDOWN | HAT_LEFTUP | HAT_LEFTDOWN
  deriving (Eq,Show)

instance Enum HatState where
  fromEnum HAT_CENTERD = 0x00

  fromEnum HAT_UP      = 0x01
  fromEnum HAT_RIGHT   = 0x02
  fromEnum HAT_DOWN    = 0x04
  fromEnum HAT_LEFT    = 0x08

  fromEnum HAT_RIGHTUP   = 0x03
  fromEnum HAT_RIGHTDOWN = 0x06
  fromEnum HAT_LEFTUP    = 0x09
  fromEnum HAT_LEFTDOWN  = 0x0C

  toEnum 0x00 = HAT_CENTERD

  toEnum 0x01 = HAT_UP
  toEnum 0x02 = HAT_RIGHT
  toEnum 0x04 = HAT_DOWN
  toEnum 0x08 = HAT_LEFT

  toEnum 0x03 = HAT_RIGHTUP
  toEnum 0x06 = HAT_RIGHTDOWN
  toEnum 0x09 = HAT_LEFTUP
  toEnum 0x0C = HAT_LEFTDOWN

----------

numJoysticks :: IO Int
numJoysticks = inHSDLNumJoysticks

joystickName :: Int -> IO String
joystickName n = do
  pstr <- inHSDLJoystickName n
  peekCString pstr

joystickOpen :: Int -> IO Joystick
joystickOpen = inHSDLJoystickOpen

joystickOpened :: Int -> IO Bool
joystickOpened n = liftM toBool $ inHSDLJoystickOpened n

joystickIndex :: Joystick -> IO Int
joystickIndex = inHSDLJoystickIndex

joystickNumAxes :: Joystick -> IO Int
joystickNumAxes = inHSDLJoystickNumAxes

joystickNumBalls :: Joystick -> IO Int
joystickNumBalls = inHSDLJoystickNumBalls

joystickNumHats :: Joystick -> IO Int
joystickNumHats = inHSDLJoystickNumHats

joystickNumButtons :: Joystick -> IO Int
joystickNumButtons = inHSDLJoystickNumButtons

joystickUpdate :: IO ()
joystickUpdate = inHSDLJoystickUpdate

joystickGetAxis :: Joystick -> Int -> IO Int
joystickGetAxis j n = liftM fromEnum $ inHSDLJoystickGetAxis j n

joystickGetHat :: Joystick -> Int -> IO HatState
joystickGetHat j n = liftM (toEnum.fromEnum) $ inHSDLJoystickGetHat j n

joystickGetButton :: Joystick -> Int -> IO Bool
joystickGetButton j n = liftM toBool $ inHSDLJoystickGetButton j n

joystickGetBall :: Joystick -> Int -> IO (Maybe Point)
joystickGetBall j n =
  alloca $ \pdx ->
  alloca $ \pdy -> do
    ret <- inHSDLJoystickGetBall j n pdx pdy
    dx  <- peek pdx
    dy  <- peek pdy
    return $ if ret==0 then Just $ pt dx dy
               else Nothing

joystickClose :: Joystick -> IO ()
joystickClose = inHSDLJoystickClose

----------

#include <HSDL.h>
#undef main

foreign import ccall "HSDL.h HSDL_NumJoysticks"       inHSDLNumJoysticks       :: IO Int
foreign import ccall "HSDL.h HSDL_JoystickName"       inHSDLJoystickName       :: Int -> IO CString
foreign import ccall "HSDL.h HSDL_JoystickOpen"       inHSDLJoystickOpen       :: Int -> IO Joystick
foreign import ccall "HSDL.h HSDL_JoystickOpened"     inHSDLJoystickOpened     :: Int -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickIndex"      inHSDLJoystickIndex      :: Joystick -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickNumAxes"    inHSDLJoystickNumAxes    :: Joystick -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickNumBalls"   inHSDLJoystickNumBalls   :: Joystick -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickNumHats"    inHSDLJoystickNumHats    :: Joystick -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickNumButtons" inHSDLJoystickNumButtons :: Joystick -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickUpdate"     inHSDLJoystickUpdate     :: IO ()
foreign import ccall "HSDL.h HSDL_JoystickGetAxis"    inHSDLJoystickGetAxis    :: Joystick -> Int -> IO Int16
foreign import ccall "HSDL.h HSDL_JoystickGetHat"     inHSDLJoystickGetHat     :: Joystick -> Int -> IO Word8
foreign import ccall "HSDL.h HSDL_JoystickGetButton"  inHSDLJoystickGetButton  :: Joystick -> Int -> IO Word8
foreign import ccall "HSDL.h HSDL_JoystickGetBall"    inHSDLJoystickGetBall    :: Joystick -> Int -> Ptr Int -> Ptr Int -> IO Int
foreign import ccall "HSDL.h HSDL_JoystickClose"      inHSDLJoystickClose      :: Joystick -> IO ()
