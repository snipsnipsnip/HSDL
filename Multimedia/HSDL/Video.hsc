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
getVideoSurface = inHSDLGetVideoSurface >>= ptrToSurface

getVideoInfo :: IO VideoInfo
getVideoInfo = inHSDLGetVideoInfo >>= toVideoInfo

videoDriverName :: IO (Maybe String)
videoDriverName =
  withCStringLen (replicate 256 '\0') $ \(cstr,len) -> do
    ret <- inHSDLVideoDriverName cstr len
    if ret==nullPtr then return Nothing
      else do
        str <- peekCString ret
        return $ Just str

data ModeList =
  AnyDimension | DimensionList [Rect]
  deriving (Eq,Show)

listModes :: Maybe PixelFormat -> [SurfaceFlag] -> IO ModeList
listModes Nothing sf = do
  ret <- inHSDLListModes nullPtr $ fromFlags sf
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
  inHSDLVideoModeOK width height depth $ fromFlags sf

setVideoMode :: Int -> Int -> Int -> [SurfaceFlag] -> IO Surface
setVideoMode width height depth sf =
  (inHSDLSetVideoMode width height depth $ fromFlags sf) >>= ptrToSurface

updateRect :: Surface -> Rect -> IO ()
updateRect sur (Rect x y w h) =
  inHSDLUpdateRect (surfaceToPtr sur) (toEnum x) (toEnum y) (toEnum w) (toEnum h)

updateRects :: Surface -> [Rect] -> IO ()
updateRects sur rs =
  withArray rs $ inHSDLUpdateRects (surfaceToPtr sur) (length rs)

flipSurface :: Surface -> IO Bool
flipSurface sur = do
  ret <- inHSDLFlip (surfaceToPtr sur)
  return $ ret==0

setColors :: Surface -> [Color] -> Int -> IO Bool
setColors sur cols fst =
  withArray cols $ \p -> do
    ret <- inHSDLSetColors (surfaceToPtr sur) p fst (length cols)
    return $ ret==1

setPalette :: Surface -> [PaletteFlag] -> [Color] -> Int -> IO Bool
setPalette sur pf cols fst =
  withArray cols $ \p -> do
    ret <- inHSDLSetPalette (surfaceToPtr sur) (fromFlags pf) p fst (length cols)
    return $ ret==1

setGamma :: Double -> Double -> Double -> IO Bool
setGamma r g b = do
  ret <- inHSDLSetGamma (realToFrac r) (realToFrac g) (realToFrac b)
  return $ ret /= -1

getGammaRamp :: IO (Bool,[(Word16,Word16,Word16)])
getGammaRamp =
  allocaArray (256*3) $ \p -> do
    ret <- inHSDLGetGammaRamp p (p `advancePtr` 256) (p `advancePtr` 512)
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
    ret <- inHSDLSetGammaRamp rp gp bp
    return $ ret /= -1

mapRGB :: PixelFormat -> Color -> IO Word32
mapRGB pf (Color r g b _) =
  with pf $ \p -> inHSDLMapRGB p r g b

mapRGBA :: PixelFormat -> Color -> IO Word32
mapRGBA pf (Color r g b a) =
  with pf $ \p -> inHSDLMapRGBA p r g b a

getRGB :: PixelFormat -> Word32 -> IO Color
getRGB pf col =
  with pf $ \ppf ->
  with 0  $ \pr  ->
  with 0  $ \pg  ->
  with 0  $ \pb  -> do
    inHSDLGetRGB col ppf pr pg pb
    liftM3 color (peek pr) (peek pg) (peek pb)

getRGBA :: PixelFormat -> Word32 -> IO Color
getRGBA pf col =
  with pf $ \ppf ->
  with 0  $ \pr  ->
  with 0  $ \pg  ->
  with 0  $ \pb  ->
  with 0  $ \pa  -> do
    inHSDLGetRGBA col ppf pr pg pb pa
    liftM4 colorA (peek pr) (peek pg) (peek pb) (peek pa)

createRGBSurface :: [SurfaceFlag] -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurface sf width height depth rmask gmask bmask amask = do
  ptr <- inHSDLCreateRGBSurface (fromFlags sf) width height depth rmask gmask bmask amask
  ptrToSurface ptr

createRGBSurfaceFrom :: Ptr () -> Int -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask = do
  ptr <- inHSDLCreateRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask
  ptrToSurface ptr

freeSurface :: Surface -> IO ()
freeSurface sur =
  inHSDLFreeSurface $ surfaceToPtr sur

lockSurface :: Surface -> IO Bool
lockSurface sur = do
  ret <- inHSDLLockSurface $ surfaceToPtr sur
  return $ ret==0

unlockSurface :: Surface -> IO ()
unlockSurface sur =
  inHSDLUnlockSurface $ surfaceToPtr sur

loadBMP :: String -> IO Surface
loadBMP file =
  withCString file $ \cstr -> do
    rw <- withCString "rb" $ inHSDLRWFromFile cstr
    inHSDLLoadBMPRW rw 1 >>= ptrToSurface

saveBMP :: Surface -> String -> IO Bool
saveBMP sur file =
  withCString file $ \cstr -> do
    rw <- withCString "wb" $ inHSDLRWFromFile cstr
    ret <- inHSDLSaveBMPRW (surfaceToPtr sur) rw 1
    return $ ret==0

setColorKey :: Surface -> [SurfaceFlag] -> Word32 -> IO Bool
setColorKey sur sf key = do
  ret <- inHSDLSetColorKey (surfaceToPtr sur) (fromFlags sf) key
  return $ ret==0

setAlpha :: Surface -> [SurfaceFlag] -> Word8 -> IO Bool
setAlpha sur sf alpha = do
  ret <- inHSDLSetAlpha (surfaceToPtr sur) (fromFlags sf) alpha
  return $ ret==0

setClipRect :: Surface -> Rect -> IO ()
setClipRect sur rc =
  with rc $ inHSDLSetClipRect (surfaceToPtr sur)

getClipRect :: Surface -> IO Rect
getClipRect sur =
  let rc = Rect 0 0 0 0 in
  with rc $ \p -> do
    inHSDLGetClipRect (surfaceToPtr sur) p
    peek p

convertSurface :: Surface -> PixelFormat -> [SurfaceFlag] -> IO Surface
convertSurface sur pf sf =
  with pf $ \p -> do
    ret <- inHSDLConvertSurface (surfaceToPtr sur) p (fromFlags sf)
    ptrToSurface ret

blitSurface :: Surface -> Maybe Rect -> Surface -> Point -> IO Int
blitSurface src sr dest pos = do
  psr <- case sr of Nothing -> return nullPtr
                    Just sr -> new sr
  pdr <- new (rect pos (sz 0 0))
  ret <- inHSDLBlitSurface (surfaceToPtr src) psr (surfaceToPtr dest) pdr
  free psr
  free pdr
  return ret

fillRect :: Surface -> Maybe Rect -> Word32 -> IO Bool
fillRect sur (Just rc) col =
  with rc $ \prc -> intFill sur prc col
fillRect sur Nothing col = intFill sur nullPtr col

intFill sur prc col = do
  ret <- inHSDLFillRect (surfaceToPtr sur) prc col
  return $ ret==0

displayFormat :: Surface -> IO Surface
displayFormat sur = do
  ret <- inHSDLDisplayFormat $ surfaceToPtr sur
  ptrToSurface ret

displayFormatAlpha :: Surface -> IO Surface
displayFormatAlpha sur = do
  ret <- inHSDLDisplayFormatAlpha $ surfaceToPtr sur
  ptrToSurface ret

newtype Cursor = Cursor (Ptr ())
ptrToCursor p = Cursor p
cursorToPtr (Cursor p) = p

warpMouse :: Point -> IO ()
warpMouse (Point x y) = inHSDLWarpMouse (toEnum x) (toEnum y)

createCursor :: [Bool] -> [Bool] -> Size -> Point -> IO Cursor
createCursor dat mask (Size w h) (Point x y) = do
  let dd = map toByte $ split 8 dat
  let mm = map toByte $ split 8 mask
  withArray dd $ \pd ->
    withArray mm $ \pm -> do
    ptr <- inHSDLCreateCursor pd pm w h x y
    return $ ptrToCursor ptr
  where
    split n = map (take n) . takeWhile (not.null) . iterate (drop n)
    toByte = inner 7 where
      inner _ [] = 0
      inner d (x:xs) = (if x then bit d else 0) .|. (inner (d-1) xs)

freeCursor :: Cursor -> IO ()
freeCursor cur = inHSDLFreeCursor $ cursorToPtr cur

setCursor :: Cursor -> IO ()
setCursor cur = inHSDLSetCursor $ cursorToPtr cur

getCursor :: IO Cursor
getCursor = do
  ptr <- inHSDLGetCursor
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
  ret <- inHSDLShowCursor $ fromEnum cs
  return $ toEnum ret

-- glLoadLibrary
-- glGetProcAddress -- ‚±‚êAŒ^•t‚©‚ñ‚Å‚µ‚åc?

glGetAttribute :: GLAttr -> IO (Bool,Int)
glGetAttribute a =
  alloca $ \p -> do
    ret <- inHSDL_GL_GetAttribute (fromEnum a) p
    d   <- peek p
    return (ret==0,d)

glSetAttribute :: GLAttr -> Int -> IO Bool
glSetAttribute a d = do
  ret <- inHSDL_GL_SetAttribute (fromEnum a) d
  return $ ret==0

glSwapBuffers :: IO ()
glSwapBuffers = inHSDL_GL_SwapBuffers

-- sdlCreateYUVOverlay
-- sdlLockYUVOverlay
-- sdlUnlockYUVOverlay
-- sdlDisplayYUVOverlay
-- sdlFreeYUVOverlay

--------

#include <HSDL.h>
#undef main

foreign import ccall "HSDL.h HSDL_RWFromFile"     inHSDLRWFromFile     :: CString -> CString -> IO RWops

foreign import ccall "HSDL.h HSDL_GetVideoSurface"  inHSDLGetVideoSurface  :: IO (Ptr ())
foreign import ccall "HSDL.h HSDL_GetVideoInfo"     inHSDLGetVideoInfo     :: IO (Ptr ())
foreign import ccall "HSDL.h HSDL_VideoDriverName"  inHSDLVideoDriverName  :: CString -> Int -> IO CString
foreign import ccall "HSDL.h HSDL_ListModes"        inHSDLListModes        :: Ptr () -> Word32 -> IO (Ptr (Ptr Rect))
foreign import ccall "HSDL.h HSDL_VideoModeOK"      inHSDLVideoModeOK      :: Int -> Int -> Int -> Word32 -> IO Int
foreign import ccall "HSDL.h HSDL_SetVideoMode"     inHSDLSetVideoMode     :: Int -> Int -> Int -> Word32 -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_UpdateRect"       inHSDLUpdateRect       :: Ptr () -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
foreign import ccall "HSDL.h HSDL_UpdateRects"      inHSDLUpdateRects      :: Ptr () -> Int -> Ptr Rect -> IO ()
foreign import ccall "HSDL.h HSDL_Flip"             inHSDLFlip             :: Ptr () -> IO Int
foreign import ccall "HSDL.h HSDL_SetColors"        inHSDLSetColors        :: Ptr () -> Ptr Color -> Int -> Int -> IO Int
foreign import ccall "HSDL.h HSDL_SetPalette"       inHSDLSetPalette       :: Ptr () -> Int -> Ptr Color -> Int -> Int -> IO Int
foreign import ccall "HSDL.h HSDL_SetGamma"         inHSDLSetGamma         :: CFloat -> CFloat -> CFloat -> IO Int
foreign import ccall "HSDL.h HSDL_GetGammaRamp"     inHSDLGetGammaRamp     :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO Int
foreign import ccall "HSDL.h HSDL_SetGammaRamp"     inHSDLSetGammaRamp     :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO Int
foreign import ccall "HSDL.h HSDL_MapRGB"           inHSDLMapRGB           :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "HSDL.h HSDL_MapRGBA"          inHSDLMapRGBA          :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "HSDL.h HSDL_GetRGB"           inHSDLGetRGB           :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "HSDL.h HSDL_GetRGBA"          inHSDLGetRGBA          :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "HSDL.h HSDL_CreateRGBSurface" inHSDLCreateRGBSurface :: Word32 -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 ->Word32 -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_CreateRGBSurfaceFrom" inHSDLCreateRGBSurfaceFrom :: Ptr () -> Int -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_FreeSurface"      inHSDLFreeSurface      :: Ptr () -> IO ()
foreign import ccall "HSDL.h HSDL_LockSurface"      inHSDLLockSurface      :: Ptr () -> IO Int
foreign import ccall "HSDL.h HSDL_UnlockSurface"    inHSDLUnlockSurface    :: Ptr () -> IO ()
foreign import ccall "HSDL.h HSDL_LoadBMP_RW"       inHSDLLoadBMPRW        :: RWops -> Int -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_SaveBMP_RW"       inHSDLSaveBMPRW        :: Ptr () -> RWops -> Int -> IO Int
foreign import ccall "HSDL.h HSDL_SetColorKey"      inHSDLSetColorKey      :: Ptr () -> Word32 -> Word32 -> IO Int
foreign import ccall "HSDL.h HSDL_SetAlpha"         inHSDLSetAlpha         :: Ptr () -> Word32 -> Word8 -> IO Int
foreign import ccall "HSDL.h HSDL_SetClipRect"      inHSDLSetClipRect      :: Ptr () -> Ptr Rect -> IO ()
foreign import ccall "HSDL.h HSDL_GetClipRect"      inHSDLGetClipRect      :: Ptr () -> Ptr Rect -> IO ()
foreign import ccall "HSDL.h HSDL_ConvertSurface"   inHSDLConvertSurface   :: Ptr () -> Ptr PixelFormat -> Word32 -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_UpperBlit"        inHSDLBlitSurface      :: Ptr () -> Ptr Rect -> Ptr () -> Ptr Rect -> IO Int
foreign import ccall "HSDL.h HSDL_FillRect"         inHSDLFillRect         :: Ptr () -> Ptr Rect -> Word32 -> IO Int
foreign import ccall "HSDL.h HSDL_DisplayFormat"    inHSDLDisplayFormat    :: Ptr () -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_DisplayFormatAlpha" inHSDLDisplayFormatAlpha :: Ptr () -> IO (Ptr ())

foreign import ccall "HSDL.h HSDL_WarpMouse"        inHSDLWarpMouse        :: Word16 -> Word16 -> IO ()
foreign import ccall "HSDL.h HSDL_CreateCursor"     inHSDLCreateCursor     :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> Int -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_FreeCursor"       inHSDLFreeCursor       :: Ptr () -> IO ()
foreign import ccall "HSDL.h HSDL_SetCursor"        inHSDLSetCursor        :: Ptr () -> IO ()
foreign import ccall "HSDL.h HSDL_GetCursor"        inHSDLGetCursor        :: IO (Ptr ())
foreign import ccall "HSDL.h HSDL_ShowCursor"       inHSDLShowCursor       :: Int -> IO Int

foreign import ccall "HSDL.h HSDL_GL_LoadLibrary"    inHSDL_GL_LoadLibrary    :: CString -> IO Int
foreign import ccall "HSDL.h HSDL_GL_GetProcAddress" inHSDL_GL_GetProcAddress :: CString -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_GL_GetAttribute"   inHSDL_GL_GetAttribute   :: Int -> Ptr Int -> IO Int
foreign import ccall "HSDL.h HSDL_GL_SetAttribute"   inHSDL_GL_SetAttribute   :: Int -> Int -> IO Int
foreign import ccall "HSDL.h HSDL_GL_SwapBuffers"    inHSDL_GL_SwapBuffers    :: IO ()

foreign import ccall "HSDL.h HSDL_CreateYUVOverlay"  inHSDL_CreateYUVOverlay  :: Int -> Int -> Word32 -> Ptr () -> IO (Ptr ())
foreign import ccall "HSDL.h HSDL_LockYUVOverlay"    inHSDL_LockYUVOverlay    :: Ptr () -> IO Int
foreign import ccall "HSDL.h HSDL_UnlockYUVOverlay"  inHSDL_UnlockYUVOverlay  :: Ptr () -> IO ()
foreign import ccall "HSDL.h HSDL_DisplayYUVOverlay" inHSDL_DisplayYUVOverlay :: Ptr () -> Ptr () -> IO Int
foreign import ccall "HSDL.h HSDL_FreeYUVOverlay"    inHSDL_FreeYUVOverlay    :: Ptr () -> IO ()
