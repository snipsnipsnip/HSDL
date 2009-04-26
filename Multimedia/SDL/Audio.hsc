{-# OPTIONS -fglasgow-exts #-}

module Multimedia.SDL.Audio(
  AudioCallback,

  AudioSpec(..),
  AudioDataFormat(..),
  AudioStatus(..),
  AudioData(..),

  audioIsEnd,
  audioAdvance,
  audioSpec,

  openAudio,
  pauseAudio,
  getAudioStatus,
  loadWAV,
  freeWAV,

-- mixAudio どうするか
  convertAudio,

  lockAudio,
  unlockAudio,
  withLockAudio,

  closeAudio,
) where

import Foreign
import Foreign.C
import Multimedia.SDL.Util

----------

type AudioCallback         = Int -> IO [AudioData]
type InternalAudioCallback = Ptr () -> Ptr Word8 -> Int -> IO ()

data AudioSpec = AudioSpec
  { asFreq     :: Int
  , asFormat   :: AudioDataFormat
  , asChannels :: Int
  , asSilence  :: Int
  , asSamples  :: Int
  , asSize     :: Word32
  , asCallback :: FunPtr InternalAudioCallback
  , asUserdata :: Ptr ()
  }

instance Storable AudioSpec where
  sizeOf    _ = 24
  alignment _ =  4

  peek p = do
    (fr :: Int)    <- peekByteOff p 0
    (fo :: Word16) <- peekByteOff p 4
    (ch :: Word8)  <- peekByteOff p 6
    (sl :: Word8)  <- peekByteOff p 7
    (sa :: Word16) <- peekByteOff p 8
    (si :: Word32) <- peekByteOff p 12
    (cb :: FunPtr InternalAudioCallback) <- peekByteOff p 16
    (ud :: Ptr ()) <- peekByteOff p 20
    return $ AudioSpec fr ((toEnum.fromEnum) fo) (fromEnum ch) (fromEnum sl) (fromEnum sa) si cb ud

  poke p (AudioSpec fr fo ch sl sa si cb ud) = do
    pokeByteOff p  0 fr
    pokeByteOff p  4 $ (((toEnum.fromEnum) fo) :: Word16)
    pokeByteOff p  6 $ ((toEnum ch) :: Word8)
    pokeByteOff p  7 $ ((toEnum sl) :: Word8)
    pokeByteOff p  8 $ ((toEnum sa) :: Word16)
    pokeByteOff p 12 $ si
    pokeByteOff p 16 cb
    pokeByteOff p 20 ud

data AudioDataFormat =
    AUDIO_U8 | AUDIO_S8
  | AUDIO_U16 | AUDIO_U16LSB
  | AUDIO_S16 | AUDIO_S16LSB
  | AUDIO_U16MSB | AUDIO_S16MSB
  | AUDIO_U16SYS | AUDIO_S16SYS
  deriving (Eq,Show)

instance Enum AudioDataFormat where
  fromEnum AUDIO_U8     = 0x0008
  fromEnum AUDIO_S8     = 0x8008

  fromEnum AUDIO_U16    = 0x0010
  fromEnum AUDIO_U16LSB = 0x0010
  fromEnum AUDIO_U16MSB = 0x1010
  fromEnum AUDIO_U16SYS = 0x0010 -- TODO: エンディアン判定

  fromEnum AUDIO_S16    = 0x8010
  fromEnum AUDIO_S16LSB = 0x8010
  fromEnum AUDIO_S16MSB = 0x9010
  fromEnum AUDIO_S16SYS = 0x8010 -- TODO:エンディアン判定

  toEnum 0x0008 = AUDIO_U8
  toEnum 0x8008 = AUDIO_S8

  toEnum 0x0010 = AUDIO_U16LSB
  toEnum 0x1010 = AUDIO_U16MSB

  toEnum 0x8010 = AUDIO_S16LSB
  toEnum 0x9010 = AUDIO_S16MSB

data AudioStatus =
  AUDIO_STOPED | AUDIO_PAUSED | AUDIO_PLAYING deriving (Enum)

data AudioData =
    AudioS8  [Int8]
  | AudioU8  [Word8]
  | AudioS16 [Int16]
  | AudioU16 [Word16]

  | AudioBuf AudioBuffer

data AudioBuffer = AudioBuffer AudioSpec Bool Int Int (Ptr Word8)

audioIsEnd :: AudioData -> Bool
audioIsEnd (AudioS8  ls) = null ls
audioIsEnd (AudioU8  ls) = null ls
audioIsEnd (AudioS16 ls) = null ls
audioIsEnd (AudioU16 ls) = null ls
audioIsEnd (AudioBuf (AudioBuffer _ _ size cur _)) = size==cur

audioAdvance :: Int -> AudioData -> AudioData
audioAdvance n (AudioS8  ls) = AudioS8  $ drop n ls
audioAdvance n (AudioU8  ls) = AudioU8  $ drop n ls
audioAdvance n (AudioS16 ls) = AudioS16 $ drop (n`div`2) ls
audioAdvance n (AudioU16 ls) = AudioU16 $ drop (n`div`2) ls
audioAdvance n (AudioBuf (AudioBuffer sp tp size cur ptr)) =
  AudioBuf $ AudioBuffer sp tp size (min size (cur+n)) ptr

audioSpec :: AudioData -> Maybe AudioSpec
audioSpec (AudioBuf (AudioBuffer sp _ _ _ _)) = Just sp
audioSpec _ = Nothing

type RWops = Ptr ()

----------

openAudio :: Int -> AudioDataFormat -> Int -> Int -> AudioCallback -> IO Bool
openAudio freq format channel samples cb = do
  cbp <- makeAudioCallback cb
  with (AudioSpec freq format channel 0 samples 0 cbp nullPtr) $ \p -> do
    ret <- inSDLOpenAudio p nullPtr
    return $ ret==0

makeAudioCallback :: AudioCallback -> IO (FunPtr InternalAudioCallback)
makeAudioCallback cb = mkIAC iac where
  iac ud buf size = do
    datum <- cb size
    mapM_ (\dat -> case dat of
      AudioS8  b -> let (len,dat) = adj size b in
        withArray dat $ \p -> inSDLMixAudio buf (castPtr p) (toEnum len) 128
      AudioU8  b -> let (len,dat) = adj size b in
        withArray dat $ \p -> inSDLMixAudio buf (castPtr p) (toEnum len) 128
      AudioS16 b -> let (len,dat) = adj (size `div` 2) b in
        withArray dat $ \p -> inSDLMixAudio buf (castPtr p) (toEnum $ len*2) 128
      AudioU16 b -> let (len,dat) = adj (size `div` 2) b in
        withArray dat $ \p -> inSDLMixAudio buf (castPtr p) (toEnum $ len*2) 128

      AudioBuf (AudioBuffer _ _ as ac ab) ->
        let len = max 0 $ min (as-ac-1) size in
        inSDLMixAudio buf (advancePtr ab ac) (toEnum len) 128
      ) datum

  adj n ls = (length d,d) where
    d = take n ls

foreign import ccall "wrapper" mkIAC :: InternalAudioCallback -> IO (FunPtr InternalAudioCallback)

pauseAudio :: Int -> IO ()
pauseAudio = inSDLPauseAudio

getAudioStatus :: IO AudioStatus
getAudioStatus = do
  ret <- inSDLGetAudioStatus
  return $ toEnum ret

loadWAV :: String -> IO (Maybe AudioData)
loadWAV name =
  withCString name $ \str ->
  alloca $ \ausp ->
  alloca $ \bufp ->
  alloca $ \sizp -> do
    rw <- withCString "rb" $ inSDLRWFromFile str
    ret <- inSDLLoadWAVRW rw 1 ausp bufp sizp
    if ret==nullPtr then return $ Nothing
      else do
        aus <- peek ausp
        buf <- peek bufp
        siz <- peek sizp
        return $ Just $ AudioBuf $ AudioBuffer aus True (fromEnum siz) 0 buf

freeWAV :: AudioData -> IO ()
freeWAV (AudioBuf (AudioBuffer _ True _ _ buf)) = inSDLFreeWAV buf
freeWAV (AudioBuf (AudioBuffer _ False _ _ buf)) = free buf
freeWAV _ = return ()

-- sdlMixAudio どうするか → 多分要らん

convertAudio :: Int -> AudioDataFormat -> Int -> AudioData -> IO (Maybe AudioData)
convertAudio freq fmt ch aud = case aud of
  AudioBuf ab@(AudioBuffer aus _ size pos buf) ->
    allocaBytes 88 $ \p -> do
    bret <- inSDLBuildAudioCVT p
        (toEnum . fromEnum $ asFormat aus) (toEnum $ asChannels aus) (toEnum $ asFreq aus) 
        (toEnum . fromEnum $ fmt         ) (toEnum ch              ) (toEnum freq        )
    if (bret == -1)
     then return Nothing
     else do
      (lenMult :: Int) <- peekByteOff p 28
      let destSize = size*lenMult
      (cbuf :: Ptr Word8) <- mallocBytes destSize
      pokeByteOff p 16 cbuf
      pokeByteOff p 20 size
      copyArray cbuf buf size
      inSDLConvertAudio p
      return $ Just $ AudioBuf $ AudioBuffer (aus { asFormat = fmt, asChannels = ch, asFreq = freq }) False destSize 0 cbuf

  _ -> return $ Just aud

lockAudio :: IO ()
lockAudio = inSDLLockAudio

unlockAudio :: IO ()
unlockAudio = inSDLUnlockAudio

withLockAudio :: IO a -> IO a
withLockAudio f = do
  lockAudio
  a <- f
  unlockAudio
  return a

closeAudio :: IO ()
closeAudio = inSDLCloseAudio

----------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_RWFromFile"     inSDLRWFromFile     :: CString -> CString -> IO RWops

foreign import ccall "SDL.h SDL_OpenAudio"      inSDLOpenAudio      :: Ptr AudioSpec -> Ptr AudioSpec -> IO Int
foreign import ccall "SDL.h SDL_PauseAudio"     inSDLPauseAudio     :: Int -> IO ()
foreign import ccall "SDL.h SDL_GetAudioStatus" inSDLGetAudioStatus :: IO Int
foreign import ccall "SDL.h SDL_LoadWAV_RW"     inSDLLoadWAVRW      :: RWops -> Int -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO (Ptr AudioSpec)
foreign import ccall "SDL.h SDL_FreeWAV"        inSDLFreeWAV        :: Ptr Word8 -> IO ()

foreign import ccall "SDL.h SDL_BuildAudioCVT"  inSDLBuildAudioCVT  :: Ptr () -> Word16 -> Word8 -> Int -> Word16 -> Word8 -> Int -> IO Int
foreign import ccall "SDL.h SDL_ConvertAudio"   inSDLConvertAudio   :: Ptr () -> IO Int

foreign import ccall "SDL.h SDL_MixAudio"       inSDLMixAudio       :: Ptr Word8 -> Ptr Word8 -> Word32 -> Int -> IO ()
foreign import ccall "SDL.h SDL_LockAudio"      inSDLLockAudio      :: IO ()
foreign import ccall "SDL.h SDL_UnlockAudio"    inSDLUnlockAudio    :: IO ()
foreign import ccall "SDL.h SDL_CloseAudio"     inSDLCloseAudio     :: IO ()
