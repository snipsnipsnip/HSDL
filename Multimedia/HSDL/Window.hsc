{-# OPTIONS -fglasgow-exts #-}
module Multimedia.HSDL.Window(
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

import Multimedia.HSDL.Util
import Multimedia.HSDL.Video

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
  inHSDLWMSetCaption ctitle cicon

getCaption :: IO (String,String)
getCaption =
  alloca $ \ptitle ->
  alloca $ \picon  -> do
    inHSDLWMGetCaption ptitle picon
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
  inHSDLWMSetIcon (surfaceToPtr icon) pmask
  free pmask

iconifyWindow :: IO Bool
iconifyWindow =
  liftM toBool inHSDLWMIconifyWindow

toggleFullScreen :: Surface -> IO Bool
toggleFullScreen sur =
  liftM toBool $ inHSDLWMToggleFullScreen (surfaceToPtr sur)

grabInput :: GrabMode -> IO GrabMode
grabInput mode = do
  liftM toEnum $ inHSDLWMGrabInput $ fromEnum mode

----------

#include <HSDL.h>
#undef main

foreign import ccall "HSDL.h HSDL_WM_SetCaption" inHSDLWMSetCaption :: CString -> CString -> IO ()
foreign import ccall "HSDL.h HSDL_WM_GetCaption" inHSDLWMGetCaption :: Ptr CString -> Ptr CString -> IO ()
foreign import ccall "HSDL.h HSDL_WM_SetIcon"    inHSDLWMSetIcon    :: Ptr () -> Ptr Word8 -> IO ()
foreign import ccall "HSDL.h HSDL_WM_IconifyWindow"    inHSDLWMIconifyWindow    :: IO Int
foreign import ccall "HSDL.h HSDL_WM_ToggleFullScreen" inHSDLWMToggleFullScreen :: Ptr () -> IO Int
foreign import ccall "HSDL.h HSDL_WM_GrabInput"  inHSDLWMGrabInput  :: Int -> IO Int
