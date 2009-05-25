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
import Multimedia.HSDL.Event
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
numJoysticks = inSDLNumJoysticks

joystickName :: Int -> IO String
joystickName n = do
  pstr <- inSDLJoystickName n
  peekCString pstr

joystickOpen :: Int -> IO Joystick
joystickOpen = inSDLJoystickOpen

joystickOpened :: Int -> IO Bool
joystickOpened n = liftM toBool $ inSDLJoystickOpened n

joystickIndex :: Joystick -> IO Int
joystickIndex = inSDLJoystickIndex

joystickNumAxes :: Joystick -> IO Int
joystickNumAxes = inSDLJoystickNumAxes

joystickNumBalls :: Joystick -> IO Int
joystickNumBalls = inSDLJoystickNumBalls

joystickNumHats :: Joystick -> IO Int
joystickNumHats = inSDLJoystickNumHats

joystickNumButtons :: Joystick -> IO Int
joystickNumButtons = inSDLJoystickNumButtons

joystickUpdate :: IO ()
joystickUpdate = inSDLJoystickUpdate

joystickGetAxis :: Joystick -> Int -> IO Int
joystickGetAxis j n = liftM fromEnum $ inSDLJoystickGetAxis j n

joystickGetHat :: Joystick -> Int -> IO HatState
joystickGetHat j n = liftM (toEnum.fromEnum) $ inSDLJoystickGetHat j n

joystickGetButton :: Joystick -> Int -> IO Bool
joystickGetButton j n = liftM toBool $ inSDLJoystickGetButton j n

joystickGetBall :: Joystick -> Int -> IO (Maybe Point)
joystickGetBall j n =
  alloca $ \pdx ->
  alloca $ \pdy -> do
    ret <- inSDLJoystickGetBall j n pdx pdy
    dx  <- peek pdx
    dy  <- peek pdy
    return $ if ret==0 then Just $ pt dx dy
               else Nothing

joystickClose :: Joystick -> IO ()
joystickClose = inSDLJoystickClose

getJoystickEventState :: IO Bool
getJoystickEventState = do
  state <- inSDLJoystickEventState sdlQuery
  return $ fromIntegral state == sdlEnable

setJoystickEventState :: Bool -> IO ()
setJoystickEventState enable = do
  inSDLJoystickEventState state
  return ()
  where
  state = if enable then sdlEnable else sdlIgnore

----------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_NumJoysticks"       inSDLNumJoysticks       :: IO Int
foreign import ccall "SDL.h SDL_JoystickName"       inSDLJoystickName       :: Int -> IO CString
foreign import ccall "SDL.h SDL_JoystickOpen"       inSDLJoystickOpen       :: Int -> IO Joystick
foreign import ccall "SDL.h SDL_JoystickOpened"     inSDLJoystickOpened     :: Int -> IO Int
foreign import ccall "SDL.h SDL_JoystickIndex"      inSDLJoystickIndex      :: Joystick -> IO Int
foreign import ccall "SDL.h SDL_JoystickNumAxes"    inSDLJoystickNumAxes    :: Joystick -> IO Int
foreign import ccall "SDL.h SDL_JoystickNumBalls"   inSDLJoystickNumBalls   :: Joystick -> IO Int
foreign import ccall "SDL.h SDL_JoystickNumHats"    inSDLJoystickNumHats    :: Joystick -> IO Int
foreign import ccall "SDL.h SDL_JoystickNumButtons" inSDLJoystickNumButtons :: Joystick -> IO Int
foreign import ccall "SDL.h SDL_JoystickUpdate"     inSDLJoystickUpdate     :: IO ()
foreign import ccall "SDL.h SDL_JoystickGetAxis"    inSDLJoystickGetAxis    :: Joystick -> Int -> IO Int16
foreign import ccall "SDL.h SDL_JoystickGetHat"     inSDLJoystickGetHat     :: Joystick -> Int -> IO Word8
foreign import ccall "SDL.h SDL_JoystickGetButton"  inSDLJoystickGetButton  :: Joystick -> Int -> IO Word8
foreign import ccall "SDL.h SDL_JoystickGetBall"    inSDLJoystickGetBall    :: Joystick -> Int -> Ptr Int -> Ptr Int -> IO Int
foreign import ccall "SDL.h SDL_JoystickClose"      inSDLJoystickClose      :: Joystick -> IO ()
foreign import ccall "SDL.h SDL_JoystickEventState"    inSDLJoystickEventState    :: EventState -> IO EventState
