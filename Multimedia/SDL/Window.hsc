{-# OPTIONS -fglasgow-exts #-}
module Multimedia.SDL.Window(
  GrabMode(..),

  setCaption,
  getCaption,
  setIcon,
  iconifyWindow,
  toggleFullScreen,
  grabInput,

) where

import Foreign
import Foreign.C
import Control.Monad

import Multimedia.SDL.Util
import Multimedia.SDL.Video

data GrabMode =
    GRAB_QUERY | GRAB_OFF | GRAB_ON
  | GRAB_FULLSCREEN -- use internally?
  deriving (Eq,Show)

instance Enum GrabMode where
  fromEnum GRAB_QUERY      = -1
  fromEnum GRAB_OFF        = 0
  fromEnum GRAB_ON         = 1
  fromEnum GRAB_FULLSCREEN = 2

  toEnum (-1) = GRAB_QUERY
  toEnum    0 = GRAB_OFF
  toEnum    1 = GRAB_ON
  toEnum    2 = GRAB_FULLSCREEN

----------

setCaption :: String -> String -> IO ()
setCaption title icon = do
  ctitle <- newCString title
  cicon  <- newCString icon
  inSDLWMSetCaption ctitle cicon

getCaption :: IO (String,String)
getCaption =
  alloca $ \ptitle ->
  alloca $ \picon  -> do
    inSDLWMGetCaption ptitle picon
    ctitle <- peek ptitle
    cicon  <- peek picon
    title  <- peekCString ctitle
    icon   <- peekCString cicon
    return (title,icon)

setIcon :: Surface -> Maybe [Word8] -> IO ()
setIcon icon mask = do
  pmask <- case mask of
    Just  m -> newArray m
    Nothing -> return $ nullPtr
  inSDLWMSetIcon (surfaceToPtr icon) pmask
  free pmask

iconifyWindow :: IO Bool
iconifyWindow =
  liftM toBool inSDLWMIconifyWindow

toggleFullScreen :: Surface -> IO Bool
toggleFullScreen sur =
  liftM toBool $ inSDLWMToggleFullScreen (surfaceToPtr sur)

grabInput :: GrabMode -> IO GrabMode
grabInput mode = do
  liftM toEnum $ inSDLWMGrabInput $ fromEnum mode

----------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_WM_SetCaption" inSDLWMSetCaption :: CString -> CString -> IO ()
foreign import ccall "SDL.h SDL_WM_GetCaption" inSDLWMGetCaption :: Ptr CString -> Ptr CString -> IO ()
foreign import ccall "SDL.h SDL_WM_SetIcon"    inSDLWMSetIcon    :: Ptr () -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_WM_IconifyWindow"    inSDLWMIconifyWindow    :: IO Int
foreign import ccall "SDL.h SDL_WM_ToggleFullScreen" inSDLWMToggleFullScreen :: Ptr () -> IO Int
foreign import ccall "SDL.h SDL_WM_GrabInput"  inSDLWMGrabInput  :: Int -> IO Int
