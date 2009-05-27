{-# OPTIONS -fglasgow-exts #-}
module Multimedia.HSDL.Video(
  SurfaceFlag(..), PaletteFlag(..),
  Surface, Palette,

  surfaceFlags,
  surfaceWidth,
  surfaceHeight,
  surfacePitch,
  surfacePixels,
  surfacePixelFormat,

  surfaceToPtr,

  PixelFormat(..), VideoInfo(..), ModeList(..),

  getVideoSurface,
  getVideoInfo,
  videoDriverName,
  listModes,
  videoModeOK,
  setVideoMode,

  updateRect,
  updateRects,
  flipSurface,
  setColors,
  setPalette,

  setGamma,
  getGammaRamp,
  setGammaRamp,
  mapRGB,
  mapRGBA,
  getRGB,
  getRGBA,

  createRGBSurface,
  createRGBSurfaceFrom,
  freeSurface,
  lockSurface,
  unlockSurface,
  loadBMP,
  saveBMP,

  setColorKey,
  setAlpha,
  setClipRect,
  getClipRect,
  convertSurface,
  blitSurface,
  fillRect,
  displayFormat,
  displayFormatAlpha,

  Cursor,
  CursorState(..),

  warpMouse,
  createCursor,
  freeCursor,
  setCursor,
  getCursor,
  showCursor,

   GLAttr(..),
-- glLoadLibrary
-- glGetProcAddress

   glGetAttribute,
   glSetAttribute,
   glSwapBuffers,

-- sdlCreateYUVOverlay
-- sdlLockYUVOverlay
-- sdlUnlockYUVOverlay
-- sdlDisplayYUVOverlay
-- sdlFreeYUVOverlay
) where

import Foreign
import Foreign.C
import Foreign.Marshal
import Control.Monad
import Multimedia.HSDL.Util

data Surface = Surface
  { sfFlags       :: [SurfaceFlag]
  , sfWidth       :: Int
  , sfHeight      :: Int
  , sfPitch       :: Int
  , sfPixelFormat :: PixelFormat
  , sfPixels      :: Ptr ()
  , sfPtr         :: Ptr ()
  }

surfaceFlags :: Surface -> [SurfaceFlag]
surfaceFlags  (Surface { sfFlags  = f }) = f
surfaceWidth :: Surface -> Int
surfaceWidth  (Surface { sfWidth  = w }) = w
surfaceHeight :: Surface -> Int
surfaceHeight (Surface { sfHeight = h }) = h
surfacePitch :: Surface -> Int
surfacePitch  (Surface { sfPitch  = p }) = p
surfacePixels :: Surface -> Ptr ()
surfacePixels (Surface { sfPixels = p }) = p
surfacePixelFormat :: Surface -> PixelFormat
surfacePixelFormat (Surface { sfPixelFormat = pf }) = pf

surfaceToPtr (Surface { sfPtr = p }) = p

ptrToSurface p = do
  (flags  :: Word32)          <- peekByteOff p 0
  (ppf    :: Ptr PixelFormat) <- peekByteOff p 4
  (width  :: Int32)           <- peekByteOff p 8
  (height :: Int32)           <- peekByteOff p 12
  (pitch  :: Word16)          <- peekByteOff p 16
  (pixels :: Ptr ())          <- peekByteOff p 20
  pf                          <- peek ppf
  return $ Surface
    { sfFlags  = toFlags flags
    , sfWidth  = fromEnum width
    , sfHeight = fromEnum height
    , sfPitch  = fromEnum pitch
    , sfPixelFormat = pf
    , sfPixels = pixels
    , sfPtr    = p }

type Palette = [Color]

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

data PixelFormat = PixelFormat
  { pfPalette      :: Palette
  , pfBitPerPixel  :: Int8
  , pfBytePerPixel :: Int8
  , pfRMask, pfGMask, pfBMask, pfAMask  :: Word32
  , pfRShift,pfGShift,pfBShift,pfAShift :: Int8
  , pfRLoss, pfGLoss, pfBLoss, pfALoss  :: Int8
  , pfColorKey :: Word32
  , pfAlpha :: Word8
  }
  deriving (Eq,Show)

instance Storable PixelFormat where
  sizeOf    _ = 40
  alignment _ = 4

  peek p = do
    pp   <- peekByteOff p 0
    bipp <- peekByteOff p 4
    bypp <- peekByteOff p 5
    rlo  <- peekByteOff p 6
    glo  <- peekByteOff p 7
    blo  <- peekByteOff p 8
    alo  <- peekByteOff p 9
    rsh  <- peekByteOff p 10
    gsh  <- peekByteOff p 11
    bsh  <- peekByteOff p 12
    ash  <- peekByteOff p 13
    rma  <- peekByteOff p 16
    gma  <- peekByteOff p 20
    bma  <- peekByteOff p 24
    ama  <- peekByteOff p 28
    ck   <- peekByteOff p 32
    ap   <- peekByteOff p 36
    pal <-
      if pp/=nullPtr then do
          pn   <- peekByteOff pp 0
          peekArray pn (pp `plusPtr` 4)
      else
        return []
    return $ PixelFormat pal bipp bypp rma gma bma ama rsh gsh bsh ash rlo glo blo alo ck ap

  poke p pf = do
    let pal = pfPalette pf
    pp <- let lp = length pal in
          if lp==0 then return nullPtr
            else mallocBytes $ 4+4*lp
    pokeByteOff p 0 $ pp
    pokeByteOff p 4 $ pfBitPerPixel pf
    pokeByteOff p 5 $ pfBytePerPixel pf
    pokeByteOff p 6 $ pfRLoss pf
    pokeByteOff p 7 $ pfGLoss pf
    pokeByteOff p 8 $ pfBLoss pf
    pokeByteOff p 9 $ pfALoss pf
    pokeByteOff p 10 $ pfRShift pf
    pokeByteOff p 11 $ pfGShift pf
    pokeByteOff p 12 $ pfBShift pf
    pokeByteOff p 13 $ pfAShift pf
    pokeByteOff p 16 $ pfRMask pf
    pokeByteOff p 20 $ pfGMask pf
    pokeByteOff p 24 $ pfBMask pf
    pokeByteOff p 28 $ pfAMask pf
    pokeByteOff p 32 $ pfColorKey pf
    pokeByteOff p 36 $ pfAlpha pf
    when (pp /= nullPtr) $ do
      pokeByteOff pp 0 $ length pal
      pokeArray (castPtr (pp `plusPtr` 4)) pal

data VideoInfo = VideoInfo
  { viHWAvailable :: Bool
  , viWMAvailable :: Bool
  , viBlitHW      :: Bool
  , viBlitHWCC    :: Bool
  , viBlitHWA     :: Bool
  , viBlitSW      :: Bool
  , viBlitSWCC    :: Bool
  , viBlitSWA     :: Bool
  , viBlitFill    :: Bool
  , viVideoMem    :: Int
  , viVideoFormat :: PixelFormat
  }
  deriving(Eq,Show)

toVideoInfo :: Ptr a -> IO VideoInfo
toVideoInfo p = do
  (flags :: Word32) <- peekByteOff p 0
  (vm    :: Int   ) <- peekByteOff p 4
  pfp               <- peekByteOff p 8
  pf                <- peek pfp
  return $ VideoInfo
    { viHWAvailable = testBit flags 0
    , viWMAvailable = testBit flags 1
    , viBlitHW      = testBit flags 9
    , viBlitHWCC    = testBit flags 10
    , viBlitHWA     = testBit flags 11
    , viBlitSW      = testBit flags 12
    , viBlitSWCC    = testBit flags 13
    , viBlitSWA     = testBit flags 14
    , viBlitFill    = testBit flags 15
    , viVideoMem    = vm
    , viVideoFormat = pf
    }

data SurfaceFlag =
    SWSURFACE | HWSURFACE | ASYNCBLIT

  | ANYFORMAT | HWPALETTE | DOUBLEBUF
  | FULLSCREEN | OPENGL | OPENGLBLIT
  | RESIZABLE | NOFRAME

  | HWACCEL | SRCCOLORKEY | RLEACCELOK | RLEACCEL
  | SRCALPHA | PREALLOC
  deriving (Eq,Show,Enum)

instance Flag SurfaceFlag where
  fromFlag SWSURFACE  = 0x00000000
  fromFlag HWSURFACE  = 0x00000001
  fromFlag ASYNCBLIT  = 0x00000004

  fromFlag ANYFORMAT  = 0x10000000
  fromFlag HWPALETTE  = 0x20000000
  fromFlag DOUBLEBUF  = 0x40000000
  fromFlag FULLSCREEN = 0x80000000
  fromFlag OPENGL     = 0x00000002
  fromFlag OPENGLBLIT = 0x0000000A
  fromFlag RESIZABLE  = 0x00000010
  fromFlag NOFRAME    = 0x00000020

  fromFlag HWACCEL    = 0x00000100
  fromFlag SRCCOLORKEY= 0x00001000
  fromFlag RLEACCELOK = 0x00002000
  fromFlag RLEACCEL   = 0x00004000
  fromFlag SRCALPHA   = 0x00010000
  fromFlag PREALLOC   = 0x01000000

data PaletteFlag = LOGPAL | PHYSPAL deriving (Enum)

instance Flag PaletteFlag where
  fromFlag LOGPAL  = 0x01
  fromFlag PHYSPAL = 0x02


data GLAttr =
    GL_RED_SIZE
  | GL_GREEN_SIZE
  | GL_BLUE_SIZE
  | GL_ALPHA_SIZE
  | GL_BUFFER_SIZE
  | GL_DOUBLEBUFFER
  | GL_DEPTH_SIZE
  | GL_STENCIL_SIZE
  | GL_ACCUM_RED_SIZE
  | GL_ACCUM_GREEN_SIZE
  | GL_ACCUM_BLUE_SIZE
  | GL_ACCUM_ALPHA_SIZE
  | GL_STEREO
  | GL_MULTISAMPLEBUFFERS
  | GL_MULTISAMPLESAMPLES
  deriving (Eq,Show,Enum)

type RWops = Ptr ()

----------

getVideoSurface :: IO Surface
getVideoSurface = inSDLGetVideoSurface >>= ptrToSurface

getVideoInfo :: IO VideoInfo
getVideoInfo = inSDLGetVideoInfo >>= toVideoInfo

videoDriverName :: IO (Maybe String)
videoDriverName =
  withCStringLen (replicate 256 '\0') $ \(cstr,len) -> do
    ret <- inSDLVideoDriverName cstr len
    if ret==nullPtr then return Nothing
      else do
        str <- peekCString ret
        return $ Just str

data ModeList =
  AnyDimension | DimensionList [Rect]
  deriving (Eq,Show)

listModes :: Maybe PixelFormat -> [SurfaceFlag] -> IO ModeList
listModes Nothing sf = do
  ret <- inSDLListModes nullPtr $ fromFlags sf
  if ret==nullPtr then return $ DimensionList []
    else if (ret `plusPtr` 1)==nullPtr then return AnyDimension
    else toRects ret >>= (return . DimensionList)

  where
    toRects :: Ptr (Ptr Rect) -> IO [Rect]
    toRects p = do
      rp <- peek p
      if rp/=nullPtr then do
          rc  <- peek rp
          rcs <- toRects (p `plusPtr` 4)
          return $ rc:rcs
        else
          return []

videoModeOK :: Int -> Int -> Int -> [SurfaceFlag] -> IO Int
videoModeOK width height depth sf =
  inSDLVideoModeOK width height depth $ fromFlags sf

setVideoMode :: Int -> Int -> Int -> [SurfaceFlag] -> IO Surface
setVideoMode width height depth sf =
  (inSDLSetVideoMode width height depth $ fromFlags sf) >>= ptrToSurface

updateRect :: Surface -> Rect -> IO ()
updateRect sur (Rect x y w h) =
  inSDLUpdateRect (surfaceToPtr sur) (toEnum x) (toEnum y) (toEnum w) (toEnum h)

updateRects :: Surface -> [Rect] -> IO ()
updateRects sur rs =
  withArray rs $ inSDLUpdateRects (surfaceToPtr sur) (length rs)

flipSurface :: Surface -> IO Bool
flipSurface sur = do
  ret <- inSDLFlip (surfaceToPtr sur)
  return $ ret==0

setColors :: Surface -> [Color] -> Int -> IO Bool
setColors sur cols fst =
  withArray cols $ \p -> do
    ret <- inSDLSetColors (surfaceToPtr sur) p fst (length cols)
    return $ ret==1

setPalette :: Surface -> [PaletteFlag] -> [Color] -> Int -> IO Bool
setPalette sur pf cols fst =
  withArray cols $ \p -> do
    ret <- inSDLSetPalette (surfaceToPtr sur) (fromFlags pf) p fst (length cols)
    return $ ret==1

setGamma :: Double -> Double -> Double -> IO Bool
setGamma r g b = do
  ret <- inSDLSetGamma (realToFrac r) (realToFrac g) (realToFrac b)
  return $ ret /= -1

getGammaRamp :: IO (Bool,[(Word16,Word16,Word16)])
getGammaRamp =
  allocaArray (256*3) $ \p -> do
    ret <- inSDLGetGammaRamp p (p `advancePtr` 256) (p `advancePtr` 512)
    if ret /= -1 then do
        ar <- peekArray 256 p
        ag <- peekArray 256 $ p `advancePtr` 256
        ab <- peekArray 256 $ p `advancePtr` 512
        return (True,zip3 ar ag ab)
      else return (False,[])

setGammaRamp :: [(Word16,Word16,Word16)] -> IO Bool
setGammaRamp gt =
  let (rl,gl,bl) = unzip3 gt in
  withArray rl $ \rp ->
  withArray gl $ \gp ->
  withArray bl $ \bp -> do
    ret <- inSDLSetGammaRamp rp gp bp
    return $ ret /= -1

mapRGB :: PixelFormat -> Color -> IO Word32
mapRGB pf (Color r g b _) =
  with pf $ \p -> inSDLMapRGB p r g b

mapRGBA :: PixelFormat -> Color -> IO Word32
mapRGBA pf (Color r g b a) =
  with pf $ \p -> inSDLMapRGBA p r g b a

getRGB :: PixelFormat -> Word32 -> IO Color
getRGB pf col =
  with pf $ \ppf ->
  with 0  $ \pr  ->
  with 0  $ \pg  ->
  with 0  $ \pb  -> do
    inSDLGetRGB col ppf pr pg pb
    liftM4 Color (peek pr) (peek pg) (peek pb) (return 255)

getRGBA :: PixelFormat -> Word32 -> IO Color
getRGBA pf col =
  with pf $ \ppf ->
  with 0  $ \pr  ->
  with 0  $ \pg  ->
  with 0  $ \pb  ->
  with 0  $ \pa  -> do
    inSDLGetRGBA col ppf pr pg pb pa
    liftM4 Color (peek pr) (peek pg) (peek pb) (peek pa)

createRGBSurface :: [SurfaceFlag] -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurface sf width height depth rmask gmask bmask amask = do
  ptr <- inSDLCreateRGBSurface (fromFlags sf) width height depth rmask gmask bmask amask
  ptrToSurface ptr

createRGBSurfaceFrom :: Ptr () -> Int -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask = do
  ptr <- inSDLCreateRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask
  ptrToSurface ptr

freeSurface :: Surface -> IO ()
freeSurface sur =
  inSDLFreeSurface $ surfaceToPtr sur

lockSurface :: Surface -> IO Bool
lockSurface sur = do
  ret <- inSDLLockSurface $ surfaceToPtr sur
  return $ ret==0

unlockSurface :: Surface -> IO ()
unlockSurface sur =
  inSDLUnlockSurface $ surfaceToPtr sur

loadBMP :: String -> IO Surface
loadBMP file =
  withCString file $ \cstr -> do
    rw <- withCString "rb" $ inSDLRWFromFile cstr
    inSDLLoadBMPRW rw 1 >>= ptrToSurface

saveBMP :: Surface -> String -> IO Bool
saveBMP sur file =
  withCString file $ \cstr -> do
    rw <- withCString "wb" $ inSDLRWFromFile cstr
    ret <- inSDLSaveBMPRW (surfaceToPtr sur) rw 1
    return $ ret==0

setColorKey :: Surface -> [SurfaceFlag] -> Word32 -> IO Bool
setColorKey sur sf key = do
  ret <- inSDLSetColorKey (surfaceToPtr sur) (fromFlags sf) key
  return $ ret==0

setAlpha :: Surface -> [SurfaceFlag] -> Word8 -> IO Bool
setAlpha sur sf alpha = do
  ret <- inSDLSetAlpha (surfaceToPtr sur) (fromFlags sf) alpha
  return $ ret==0

setClipRect :: Surface -> Rect -> IO ()
setClipRect sur rc =
  with rc $ inSDLSetClipRect (surfaceToPtr sur)

getClipRect :: Surface -> IO Rect
getClipRect sur =
  let rc = Rect 0 0 0 0 in
  with rc $ \p -> do
    inSDLGetClipRect (surfaceToPtr sur) p
    peek p

convertSurface :: Surface -> PixelFormat -> [SurfaceFlag] -> IO Surface
convertSurface sur pf sf =
  with pf $ \p -> do
    ret <- inSDLConvertSurface (surfaceToPtr sur) p (fromFlags sf)
    ptrToSurface ret

blitSurface :: Surface -> Maybe Rect -> Surface -> Point -> IO Int
blitSurface src sr dest (Point x y) = do
  psr <- case sr of Nothing -> return nullPtr
                    Just sr -> new sr
  pdr <- new (Rect x y 0 0)
  ret <- inSDLBlitSurface (surfaceToPtr src) psr (surfaceToPtr dest) pdr
  free psr
  free pdr
  return ret

fillRect :: Surface -> Maybe Rect -> Word32 -> IO Bool
fillRect sur (Just rc) col =
  with rc $ \prc -> intFill sur prc col
fillRect sur Nothing col = intFill sur nullPtr col

intFill sur prc col = do
  ret <- inSDLFillRect (surfaceToPtr sur) prc col
  return $ ret==0

displayFormat :: Surface -> IO Surface
displayFormat sur = do
  ret <- inSDLDisplayFormat $ surfaceToPtr sur
  ptrToSurface ret

displayFormatAlpha :: Surface -> IO Surface
displayFormatAlpha sur = do
  ret <- inSDLDisplayFormatAlpha $ surfaceToPtr sur
  ptrToSurface ret

newtype Cursor = Cursor (Ptr ())
ptrToCursor p = Cursor p
cursorToPtr (Cursor p) = p

warpMouse :: Point -> IO ()
warpMouse (Point x y) = inSDLWarpMouse (toEnum x) (toEnum y)

createCursor :: [Bool] -> [Bool] -> Size -> Point -> IO Cursor
createCursor dat mask (Size w h) (Point x y) = do
  let dd = map toByte $ split 8 dat
  let mm = map toByte $ split 8 mask
  withArray dd $ \pd ->
    withArray mm $ \pm -> do
    ptr <- inSDLCreateCursor pd pm w h x y
    return $ ptrToCursor ptr
  where
    split n = map (take n) . takeWhile (not.null) . iterate (drop n)
    toByte = inner 7 where
      inner _ [] = 0
      inner d (x:xs) = (if x then bit d else 0) .|. (inner (d-1) xs)

freeCursor :: Cursor -> IO ()
freeCursor cur = inSDLFreeCursor $ cursorToPtr cur

setCursor :: Cursor -> IO ()
setCursor cur = inSDLSetCursor $ cursorToPtr cur

getCursor :: IO Cursor
getCursor = do
  ptr <- inSDLGetCursor
  return $ ptrToCursor ptr

data CursorState =
    CURSOR_QUERY
  | CURSOR_IGNORE
  | CURSOR_DISABLE
  | CURSOR_ENABLE
  deriving (Eq,Show)

instance Enum CursorState where
  fromEnum CURSOR_QUERY   = -1
  fromEnum CURSOR_IGNORE  = 0
  fromEnum CURSOR_DISABLE = 0
  fromEnum CURSOR_ENABLE  = 1

  toEnum (-1) = CURSOR_QUERY
  toEnum 0    = CURSOR_DISABLE
  toEnum 1    = CURSOR_ENABLE

showCursor :: CursorState -> IO CursorState
showCursor cs = do
  ret <- inSDLShowCursor $ fromEnum cs
  return $ toEnum ret

-- glLoadLibrary
-- glGetProcAddress -- ‚±‚êAŒ^•t‚©‚ñ‚Å‚µ‚åc?

glGetAttribute :: GLAttr -> IO (Bool,Int)
glGetAttribute a =
  alloca $ \p -> do
    ret <- inSDL_GL_GetAttribute (fromEnum a) p
    d   <- peek p
    return (ret==0,d)

glSetAttribute :: GLAttr -> Int -> IO Bool
glSetAttribute a d = do
  ret <- inSDL_GL_SetAttribute (fromEnum a) d
  return $ ret==0

glSwapBuffers :: IO ()
glSwapBuffers = inSDL_GL_SwapBuffers

-- sdlCreateYUVOverlay
-- sdlLockYUVOverlay
-- sdlUnlockYUVOverlay
-- sdlDisplayYUVOverlay
-- sdlFreeYUVOverlay

--------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_RWFromFile"     inSDLRWFromFile     :: CString -> CString -> IO RWops

foreign import ccall "SDL.h SDL_GetVideoSurface"  inSDLGetVideoSurface  :: IO (Ptr ())
foreign import ccall "SDL.h SDL_GetVideoInfo"     inSDLGetVideoInfo     :: IO (Ptr ())
foreign import ccall "SDL.h SDL_VideoDriverName"  inSDLVideoDriverName  :: CString -> Int -> IO CString
foreign import ccall "SDL.h SDL_ListModes"        inSDLListModes        :: Ptr () -> Word32 -> IO (Ptr (Ptr Rect))
foreign import ccall "SDL.h SDL_VideoModeOK"      inSDLVideoModeOK      :: Int -> Int -> Int -> Word32 -> IO Int
foreign import ccall "SDL.h SDL_SetVideoMode"     inSDLSetVideoMode     :: Int -> Int -> Int -> Word32 -> IO (Ptr ())
foreign import ccall "SDL.h SDL_UpdateRect"       inSDLUpdateRect       :: Ptr () -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
foreign import ccall "SDL.h SDL_UpdateRects"      inSDLUpdateRects      :: Ptr () -> Int -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_Flip"             inSDLFlip             :: Ptr () -> IO Int
foreign import ccall "SDL.h SDL_SetColors"        inSDLSetColors        :: Ptr () -> Ptr Color -> Int -> Int -> IO Int
foreign import ccall "SDL.h SDL_SetPalette"       inSDLSetPalette       :: Ptr () -> Int -> Ptr Color -> Int -> Int -> IO Int
foreign import ccall "SDL.h SDL_SetGamma"         inSDLSetGamma         :: CFloat -> CFloat -> CFloat -> IO Int
foreign import ccall "SDL.h SDL_GetGammaRamp"     inSDLGetGammaRamp     :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO Int
foreign import ccall "SDL.h SDL_SetGammaRamp"     inSDLSetGammaRamp     :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO Int
foreign import ccall "SDL.h SDL_MapRGB"           inSDLMapRGB           :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MapRGBA"          inSDLMapRGBA          :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_GetRGB"           inSDLGetRGB           :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetRGBA"          inSDLGetRGBA          :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_CreateRGBSurface" inSDLCreateRGBSurface :: Word32 -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 ->Word32 -> IO (Ptr ())
foreign import ccall "SDL.h SDL_CreateRGBSurfaceFrom" inSDLCreateRGBSurfaceFrom :: Ptr () -> Int -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr ())
foreign import ccall "SDL.h SDL_FreeSurface"      inSDLFreeSurface      :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LockSurface"      inSDLLockSurface      :: Ptr () -> IO Int
foreign import ccall "SDL.h SDL_UnlockSurface"    inSDLUnlockSurface    :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LoadBMP_RW"       inSDLLoadBMPRW        :: RWops -> Int -> IO (Ptr ())
foreign import ccall "SDL.h SDL_SaveBMP_RW"       inSDLSaveBMPRW        :: Ptr () -> RWops -> Int -> IO Int
foreign import ccall "SDL.h SDL_SetColorKey"      inSDLSetColorKey      :: Ptr () -> Word32 -> Word32 -> IO Int
foreign import ccall "SDL.h SDL_SetAlpha"         inSDLSetAlpha         :: Ptr () -> Word32 -> Word8 -> IO Int
foreign import ccall "SDL.h SDL_SetClipRect"      inSDLSetClipRect      :: Ptr () -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_GetClipRect"      inSDLGetClipRect      :: Ptr () -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_ConvertSurface"   inSDLConvertSurface   :: Ptr () -> Ptr PixelFormat -> Word32 -> IO (Ptr ())
foreign import ccall "SDL.h SDL_UpperBlit"        inSDLBlitSurface      :: Ptr () -> Ptr Rect -> Ptr () -> Ptr Rect -> IO Int
foreign import ccall "SDL.h SDL_FillRect"         inSDLFillRect         :: Ptr () -> Ptr Rect -> Word32 -> IO Int
foreign import ccall "SDL.h SDL_DisplayFormat"    inSDLDisplayFormat    :: Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_DisplayFormatAlpha" inSDLDisplayFormatAlpha :: Ptr () -> IO (Ptr ())

foreign import ccall "SDL.h SDL_WarpMouse"        inSDLWarpMouse        :: Word16 -> Word16 -> IO ()
foreign import ccall "SDL.h SDL_CreateCursor"     inSDLCreateCursor     :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> Int -> IO (Ptr ())
foreign import ccall "SDL.h SDL_FreeCursor"       inSDLFreeCursor       :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_SetCursor"        inSDLSetCursor        :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_GetCursor"        inSDLGetCursor        :: IO (Ptr ())
foreign import ccall "SDL.h SDL_ShowCursor"       inSDLShowCursor       :: Int -> IO Int

foreign import ccall "SDL.h SDL_GL_LoadLibrary"    inSDL_GL_LoadLibrary    :: CString -> IO Int
foreign import ccall "SDL.h SDL_GL_GetProcAddress" inSDL_GL_GetProcAddress :: CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GL_GetAttribute"   inSDL_GL_GetAttribute   :: Int -> Ptr Int -> IO Int
foreign import ccall "SDL.h SDL_GL_SetAttribute"   inSDL_GL_SetAttribute   :: Int -> Int -> IO Int
foreign import ccall "SDL.h SDL_GL_SwapBuffers"    inSDL_GL_SwapBuffers    :: IO ()

foreign import ccall "SDL.h SDL_CreateYUVOverlay"  inSDL_CreateYUVOverlay  :: Int -> Int -> Word32 -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_LockYUVOverlay"    inSDL_LockYUVOverlay    :: Ptr () -> IO Int
foreign import ccall "SDL.h SDL_UnlockYUVOverlay"  inSDL_UnlockYUVOverlay  :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_DisplayYUVOverlay" inSDL_DisplayYUVOverlay :: Ptr () -> Ptr () -> IO Int
foreign import ccall "SDL.h SDL_FreeYUVOverlay"    inSDL_FreeYUVOverlay    :: Ptr () -> IO ()
