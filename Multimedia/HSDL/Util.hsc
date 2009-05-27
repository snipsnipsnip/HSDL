{-# LANGUAGE ScopedTypeVariables #-}

module Multimedia.HSDL.Util(
  Flag(..),
  Point(..),
  Size(..),
  Rect(..),
  Color(..),
  EventState, sdlQuery, sdlEnable, sdlIgnore
) where

import Data.Bits
import Foreign
import Foreign.C

class (Enum f) => Flag f where
  fromFlag  :: (Bits b) => f -> b

  fromFlags :: (Bits b) => [f] -> b
  fromFlags = foldl (.|.) 0 . map fromFlag

  toFlags :: (Bits b) => b -> [f]
  toFlags n = filter (\f -> (n .&. fromFlag f) == fromFlag f) [toEnum 0 ..]

data Point = Point { pointX, pointY :: Int } deriving (Eq,Show)

data Size  = Size  { sizeW,  sizeH  :: Int } deriving (Eq,Show)

data Rect = Rect
  { rectLeft, rectTop, rectWidth, rectHeight :: Int }
  deriving (Eq,Show)

instance Storable Rect where
  sizeOf    _ = 8
  alignment _ = 4

  peek p = do
    (x :: Int16)  <- peekByteOff p 0
    (y :: Int16)  <- peekByteOff p 2
    (w :: Word16) <- peekByteOff p 4
    (h :: Word16) <- peekByteOff p 6
    return $ Rect (fromEnum x) (fromEnum y) (fromEnum w) (fromEnum h)

  poke p rc = do
    pokeByteOff p 0 $ (toEnum :: Int -> Int16)  $ rectLeft   rc
    pokeByteOff p 2 $ (toEnum :: Int -> Int16)  $ rectTop    rc
    pokeByteOff p 4 $ (toEnum :: Int -> Word16) $ rectWidth  rc
    pokeByteOff p 6 $ (toEnum :: Int -> Word16) $ rectHeight rc

data Color = Color
  { colR, colG, colB, colA :: Word8 }
  deriving (Eq,Show)

instance Storable Color where
  sizeOf    _ = 4
  alignment _ = 4

  peek p = do
    r <- peekByteOff p 0
    g <- peekByteOff p 1
    b <- peekByteOff p 2
    a <- peekByteOff p 3
    return $ Color r g b a

  poke p col = do
    pokeByteOff p 0 $ colR col
    pokeByteOff p 1 $ colR col
    pokeByteOff p 2 $ colR col
    pokeByteOff p 3 $ colR col

--

#include "SDL.h"
#undef main

type EventState = CInt
#enum EventState, id, SDL_QUERY, SDL_IGNORE, SDL_ENABLE

