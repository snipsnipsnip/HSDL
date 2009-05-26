module Multimedia.HSDL.Util(
  Flag(..),
  Point(..),
  Size(..),
  Rect(..),
  Color(..),

  point,pt,
  size,sz,
  rect,
  color, colorA,
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
point :: Int -> Int -> Point
point = Point
pt :: Int -> Int -> Point
pt    = Point

data Size  = Size  { sizeW,  sizeH  :: Int } deriving (Eq,Show)
size :: Int -> Int -> Size
size = Size
sz :: Int -> Int -> Size
sz   = Size

data Rect = Rect
  { rectLeft, rectTop, rectWidth, rectHeight :: Int }
  deriving (Eq,Show)

rect :: Point -> Size -> Rect
rect (Point x y) (Size w h) = Rect x y w h

data Color = Color
  { colR, colG, colB, colA :: Word8 }
  deriving (Eq,Show)

instance Storable Color where
  sizeOf    _ = #size SDL_Color
  alignment _ = 4

  peek p = do
    r <- (#peek SDL_Color, r) p
    g <- (#peek SDL_Color, g) p
    b <- (#peek SDL_Color, b) p
    a <- (#peek SDL_Color, unused) p
    return Color { colR = r, colG = g, colB = b, colA = a }

  poke p Color { colR = r, colG = g, colB = b, colA = a } = do
    (#poke SDL_Color, r) p r
    (#poke SDL_Color, g) p g
    (#poke SDL_Color, b) p b
    (#poke SDL_Color, unused) p a

color :: Word8 -> Word8 -> Word8 -> Color
color r g b    = Color r g b 255

colorA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
colorA r g b a = Color r g b a

--

#include "SDL.h"
#undef main

type EventState = CInt
#enum EventState, , SDL_QUERY, SDL_IGNORE, SDL_ENABLE

