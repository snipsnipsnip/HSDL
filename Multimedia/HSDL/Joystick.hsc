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

  getJoystickEventState,
  setJoystickEventState
) where

import Foreign
import Foreign.C
import Control.Monad
import Multimedia.HSDL.Event
import Multimedia.HSDL.Util

type Joystick = Ptr ()

data HatState =
    HAT_CENTERED
  | HAT_UP | HAT_RIGHT | HAT_DOWN | HAT_LEFT
  | HAT_RIGHTUP | HAT_RIGHTDOWN | HAT_LEFTUP | HAT_LEFTDOWN
  deriving (Eq,Show)

instance Enum HatState where
  fromEnum HAT_CENTERED = #const SDL_HAT_CENTERED

  fromEnum HAT_UP      = #const SDL_HAT_UP
  fromEnum HAT_RIGHT   = #const SDL_HAT_RIGHT
  fromEnum HAT_DOWN    = #const SDL_HAT_DOWN
  fromEnum HAT_LEFT    = #const SDL_HAT_LEFT

  fromEnum HAT_RIGHTUP   = #const SDL_HAT_RIGHTUP
  fromEnum HAT_RIGHTDOWN = #const SDL_HAT_RIGHTDOWN
  fromEnum HAT_LEFTUP    = #const SDL_HAT_LEFTUP
  fromEnum HAT_LEFTDOWN  = #const SDL_HAT_LEFTDOWN

  toEnum (#const SDL_HAT_CENTERED) = HAT_CENTERED

  toEnum (#const SDL_HAT_UP) = HAT_UP
  toEnum (#const SDL_HAT_RIGHT) = HAT_RIGHT
  toEnum (#const SDL_HAT_DOWN) = HAT_DOWN
  toEnum (#const SDL_HAT_LEFT) = HAT_LEFT

  toEnum (#const SDL_HAT_RIGHTUP) = HAT_RIGHTUP
  toEnum (#const SDL_HAT_RIGHTDOWN) = HAT_RIGHTDOWN
  toEnum (#const SDL_HAT_LEFTUP) = HAT_LEFTUP
  toEnum (#const SDL_HAT_LEFTDOWN) = HAT_LEFTDOWN

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

joystickGetBall :: Joystick -> Int -> IO (Maybe (Int, Int))
joystickGetBall j n =
  alloca $ \pdx ->
  alloca $ \pdy -> do
    ret <- inSDLJoystickGetBall j n pdx pdy
    dx  <- peek pdx
    dy  <- peek pdy
    return $ do
      guard $ ret == 0
      return (dx,dy)

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
